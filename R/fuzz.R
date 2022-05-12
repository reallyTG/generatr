#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom sxpdb open_db path_db
#' @export
quick_fuzz <- function(pkg_name, fn_name, db, budget, origins_db, runner, generator, quiet = !interactive()) {
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

    runner_fun <- create_fuzz_runner(runner = runner, db_path = sxpdb::path_db(value_db))

    fuzz(pkg_name, fn_name, generator, runner_fun, quiet)
}

#' @importFrom purrr map map_dfr map_chr
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom progress progress_bar
#' @export
fuzz <- function(pkg_name, fn_name, generator, runner,
                 quiet = !interactive(),
                 get_type = contractr::infer_type,
                 timeout_s = 60 * 60) {

    # returns a list
    # - args_idx: int[]       - indices to be used for the args
    # - error: chr            - the error from the function
    # - exit: int             - the exit code if the R session has crashed or 0
    # - result: any           - the function return value
    # - status: int           - 0 is OK, 1: error, 2: crash, -1: generate_args failure, -2: runner failure
    # - signature: chr          - the signature inferred from the call (using `get_type`)
    # - return_metadata: list -  sexptype, classes, length, number of attributes, number of dimensions, number of rows, presence of NA
    run_one <- function() {
        res <- list(
            args_idx = NA_integer_,
            error = NA_character_,
            exit = NA_integer_,
            result = NULL,
            signature = NA_character_,
            status = 0L
        )
        class(res) <- "result"

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
                ts <- system.time(r <- runner(pkg_name, fn_name, res$args_idx))
                res <- modifyList(res, r)
                res$ts_run <- as.numeric(ts["elapsed"])

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

            sig_args <- purrr::map_chr(res$args_idx, ~ get_type(get_value(generator, .)))
            sig_args <- paste0(sig_args, collapse = ", ")
            res$signature <- paste0("(", sig_args, ") -> ", get_type(res$result))
            res$return_metadata <- list(metadata_from_value(res$result))
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

    collected_results <- new.env(parent = emptyenv())
    i <- 1
    cont <- TRUE
    start_time <- Sys.time()
    while (cont) {
        run <- run_one()
        if (is.null(run)) {
            cont <- FALSE
        } else {
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

    y$args_idx <- paste0(y$args_idx, collapse = ";")
    y$result <- NULL

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
    ts_call <- system.time(ret <- generatr::trace_dispatch_call(fn, args))

    if (ret$status != 0) {
        ret$error <- geterrmessage()
    } else {
        ret$error <- NA_character_
    }

    ret$status <- NULL
    # so it can be put into a cell in data frame
    ret$dispatch <- list(ret$dispatch)
    ret$ts_call <- as.numeric(ts_call["elapsed"])
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
