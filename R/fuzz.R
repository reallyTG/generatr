#' @importFrom sxpdb open_db path_db
#' @export
quick_fuzz <- function(pkg_name, fn_name, db, budget, generator, quiet = !interactive()) {
    runner <- runner_start(quiet = quiet)
    on.exit(runner_stop(runner, quiet = quiet))

    value_db <- if (is.character(db)) {
        sxpdb::open_db(db)
    } else {
        db
    }

    origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble

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
                 get_type = contractr::infer_type) {

    # returns a list
    # - args_idx: int[]    - indicies to be used for the args
    # - error: chr         - the error from the function
    # - exit: int          - the exit code if the R session has crashed or 0
    # - messages: chr[]    - messages captured during the call
    # - output: chr        - output captured during the call
    # - result: any        - the function return value
    # - warnings: chr[]    - warnings captured during the call
    # - status: int        - 0 is OK, 1: warnings, 2: error, 3: crash, -1: generate_args failure, -2: runner failure
    # - signature: chr       - the signatrue inferred from the call (using `get_type`)
    run_one <- function() {
        res <- list(
            args_idx = NA_integer_,
            error = NA_character_,
            exit = NA_integer_,
            messages = NA_character_,
            output = NA_character_,
            result = NULL,
            signature = NA_character_,
            status = 0L,
            warnings = NA_character_
        )
        class(res) <- "result"

        tryCatch({
            res$args_idx <- generate_args(generator)

            if (is.null(res$args_idx)) {
                return(NULL)
            }

            if (!is.integer(res$args_idx)) {
                stop("Generated value indices are not integers!")
            }
        }, error = function(e) {
            res$error <<- paste("fuzz-generate-args:", e$message)
            res$status <<- -1L
        })

        if (res$status != 0) {
            return(tibble::as_tibble(res))
        }

        tryCatch({
            r <- runner(pkg_name, fn_name, res$args_idx)

            res$error <- r$error
            res$messages <- r$messages
            res$output <- r$output
            res$exit <- r$exit
            res$warnings <- r$warnings

            if (!is.null(r$result)) {
                res$result <- r$result
            }
            if (any(!is.na(res$warnings))) {
                res$status <- 1L
            }
            if (!is.na(res$error)) {
                res$status <- 2L
            }
            if (!is.na(res$exit)) {
                res$status <- 3L
            }
        }, error = function(e) {
            res$error <<- e$message
            res$status <<- -2L
        })

        if (res$status == 0L) {
            successful_call(generator, res$args_idx)
        }

        if (res$status %in% c(0L, 1L)) {
            sig_args <- purrr::map_chr(res$args_idx, ~get_type(get_value(generator, .)))
            sig_args <- paste0(sig_args, collapse = ", ")
            res$signature <- paste0("(", sig_args, ") -> ", get_type(res$result))
        }

        tibble::as_tibble(res)
    }

    tick <- if (!quiet) {
        pb <- progress::progress_bar$new(
            format = paste0("  fuzzing ", pkg_name, ":::", fn_name, " [:bar] :current/:total (:percent) :elapsed"),
            total = remaining(generator), clear = FALSE, width = 80
        )
        function() pb$tick()
    } else {
        function() NULL
    }

    collected_results <- new.env(parent = emptyenv())
    i <- 1
    cont <- TRUE
    while (cont) {
        run <- run_one()
        if (is.null(run)) {
            cont <- FALSE
        } else {
            assign(as.character(i), run, envir = collected_results)
            i <- i + 1
            tick()
        }
    }

    df <- dplyr::bind_rows(as.list(collected_results))
    df
}

#' @export
as_tibble.result <- function(x, ...) {
    y <- x

    fix <- function(v) {
        if (length(v) == 0) {
            NA_character_
        } else if (length(v) == 1 && !is.na(v) && v == "") {
            NA_character_
        } else {
            paste0(v, collapse = ";")
        }
    }

    y$result <- NULL
    y$args_idx <- fix(x$args_idx)
    y$output <- trimws(y$output)
    y$output <- if (is.na(y$output) || nchar(y$output) == 0) NA_character_ else y$output
    y$messages <- fix(x$messages)
    y$warnings <- fix(x$warnings)

    class(y) <- NULL
    tibble::as_tibble(y)
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

    if (ret$result != 0L) {
        stop("Unable to load DB in the runner: ", format(ret))
    }

    function(pkg_name, fn_name, args_idx) {
        runner_exec(
            runner,
            function(pkg_name, fn_name, args_idx, db_path) {
                if (!exists(".DB", envir = globalenv())) {
                    assign(".DB", sxpdb::open_db(db_path), envir = globalenv())
                }
                args <- lapply(args_idx, function(idx) sxpdb::get_value_idx(.DB, idx))
                fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")
                do.call(fn, args)
            },
            list(pkg_name, fn_name, args_idx, db_path),
            timeout_ms = timeout_ms
        )
    }
}
