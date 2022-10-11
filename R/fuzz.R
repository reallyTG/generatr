#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom sxpdb open_db path_db
#' @export
quick_fuzz <- function(pkg_name, fn_name, db, budget,
                       origins_db, runner, generator,
                       action = c("store", "infer"),
                       quiet = !interactive()) {
    action <- match.arg(action)

    runner <- if (missing(runner)) {
        tmp <- runner_start(quiet = quiet)
        on.exit(runner_stop(runner, quiet = quiet))
        tmp
    }

    value_db <- if (is.character(db)) {
        sxpdb::open_db(db)
    } else {
        db
    }

    value_db <- if (!has_search_index(db)) {
      sxpdb::close_db(db)
      db <- sxpdb::open_db(db, mode = T)
      sxpdb::build_indexes(db)
      sxpdb::close_db(db)
      sxpdb::open_db(db)
    }

    if (missing(origins_db)) {
        origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble()
    }

    if (missing(generator)) {
        generator <- create_fd_args_generator(
            pkg_name, fn_name,
            value_db = value_db, origins_db = origins_db, meta_db = NULL,
            budget = budget
        )
    }

    if (action == "store") {
        rdb_path <- tempfile()
        rdb <- sxpdb::open_db(rdb_path, mode = TRUE)
        result_processor <- generatr::store_result(rdb)
    } else if (action == "infer") {
        result_processor <- generatr::infer_fun_type
    }

    runner_fun <- create_fuzz_runner(runner = runner, db_path = sxpdb::path_db(value_db))

    ret <- fuzz(pkg_name, fn_name, generator, runner_fun, result_processor = result_processor, quiet)

    if (action == "store") {
        attr(ret, "db_path") <- rdb_path
        close_db(rdb)
    }

    ret
}

#' @export
store_result <- function(db) {
    function(args, retval) sxpdb::add_val(db, retval)
}

#' @export
infer_fun_type <- function(args, retval, get_type = contractr::infer_type) {
    sig_args <- purrr::map_chr(args, get_type)
    sig_args <- paste0(sig_args, collapse = ", ")
    paste0("(", sig_args, ") => ", get_type(retval))
}

#' @importFrom purrr map map_dfr map_chr
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom progress progress_bar
#' @export
fuzz <- function(pkg_name, fn_name, generator, runner,
                 quiet = !interactive(),
                 result_processor = infer_fun_type,
                 timeout_s = 60 * 60) {

    new_result <- function() {
        res <- list(
            args_idx = NA_integer_,
            error = NA_character_,
            exit = NA_integer_,
            result = NULL,
            status = 0L
        )
        class(res) <- "result"
        res
    }

    # returns a list
    # - args_idx: int[]       - indices to be used for the args
    # - error: chr            - the error from the function
    # - exit: int             - the exit code if the R session has crashed or 0
    # - status: int           - 0 is OK, 1: error, 2: crash, -1: generate_args failure, -2: runner failure
    # - result: any           - the return value of calling the result_processor
    run_one <- function() {
        res <- new_result()
        tryCatch(
            {
                res$args_idx <- generate_args(generator)

                if (is.null(res$args_idx)) {
                    return(NULL)
                }

                if (!is.integer(res$args_idx)) {
                    stop("Generated value indices are not integers!")
                }
            },
            error = function(e) {
                res$error <<- paste("fuzz-generate-args:", e$message)
                res$status <<- -1L
            }
        )

        if (res$status != 0) {
            return(tibble::as_tibble(res))
        }

        tryCatch(
            {
                r <- runner(pkg_name, fn_name, res$args_idx)
                res <- modifyList(res, r)
                if (is.null(r$result)) {
                    res["result"] <- list(NULL)
                }

                if (!is.na(res$error)) {
                    res$status <- 1L
                }
                if (!is.na(res$exit)) {
                    res$status <- 2L
                }
            },
            error = function(e) {
                res$error <<- e$message
                res$status <<- -2L
            }
        )

        if (res$status == 0L) {
            successful_call(generator, res$args_idx)
            args <- purrr::map(res$args_idx, ~ get_value(generator, .))
            res$result <- result_processor(args, res$result)
        }

        tibble::as_tibble(res)
    }

    tick <- if (!quiet) {
        pb <- progress::progress_bar$new(
            format = paste0("  fuzzing ", pkg_name, ":::", fn_name, " [:bar] :current/:total (:percent) :elapsed"),
            force = TRUE,
            total = remaining(generator), clear = FALSE, width = 80
        )
        function() pb$tick()
    } else {
        function() NULL
    }

    # TODO: use set instead
    collected_results <- new.env(parent = emptyenv())
    i <- 1
    cont <- TRUE
    start_time <- Sys.time()
    while (cont) {
        ts <- Sys.time()
        run <- tryCatch({
            run_one()
        }, error = function(e) {
            res <- new_result()
            res$error <- e$message
            res$status <- -3L
            as_tibble(res)
        })
        ts <- Sys.time() - ts

        if (is.null(run)) {
            cont <- FALSE
        } else {
            run$ts <- ts
            assign(as.character(i), run, envir = collected_results)
            i <- i + 1
            tick()
            if (Sys.time() - start_time > timeout_s) {
                cont <- FALSE
                warning("Timeout (", timeout_s, " sec) reached after ", i, " calls")
            }
        }
    }

    df <- dplyr::bind_rows(as.list(collected_results))
    df
}

#' @export
as_tibble.result <- function(x, ...) {
    y <- x

    y$args_idx <- list(y$args_idx)
    if (is.null(x$result)) {
        y$result <- NA
    }

    class(y) <- NULL
    tibble::as_tibble(y)
}

invoke_fun <- function(pkg_name, fn_name, args_idx, db_path) {
    if (!exists(".DB", envir = globalenv())) {
        assign(".DB", sxpdb::open_db(db_path), envir = globalenv())
    }

    args <- lapply(args_idx, function(idx) sxpdb::get_value_idx(.DB, idx))
    fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")

    options(warn = 2)
    ret <- generatr::trace_dispatch_call(fn, args)

    if (ret$status != 0) {
        ret$error <- geterrmessage()
    } else {
        ret$error <- NA_character_
    }

    ret$status <- NULL
    # so it can be put into a cell in data frame
    ret$dispatch <- list(ret$dispatch)
    ret
}

#' @export
create_fuzz_runner <- function(db_path, runner, timeout_ms = 60 * 1000) {
    # load db in the worker
    ret <- runner_exec(
        runner,
        function(db_path) {
            assign(".DB", sxpdb::open_db(db_path), envir = globalenv())
            0L
        },
        list(db_path)
    )

    if (!is.integer(ret$result) || ret$result != 0L) {
        stop("Unable to load DB in the runner: (error=", ret$error, ", exit=", ret$exit, ")")
    }

    function(pkg_name, fn_name, args_idx) {
        runner_exec(
            runner,
            generatr:::invoke_fun,
            list(pkg_name, fn_name, args_idx, db_path),
            timeout_ms = timeout_ms,
            capture = FALSE
        )
    }
}
