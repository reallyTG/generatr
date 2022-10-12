#' @export
runner_start <- function(lib_loc = .libPaths(), quiet = TRUE) {
    runner <- new.env(parent = emptyenv())
    runner$sess <- runner_create(lib_loc = lib_loc, quiet = quiet)
    runner$lib_loc <- lib_loc
    class(runner) <- "runner"
    runner
}

#' @importFrom callr r_session
runner_create <- function(lib_loc = .libPaths(), quiet = TRUE) {
    sess <- callr::r_session$new(
        wait = TRUE,
        options = callr::r_session_options(libpath = lib_loc)
    )

    if (sess$get_state() != "idle") {
        stop("Unable to start R session: ", sess)
    }

    if (!quiet) {
        message("Started a new runner:", format(sess))
    }
    sess
}

#' @export
runner_stop <- function(runner, quiet = TRUE) {
    sess <- runner$sess
    if (sess$get_state() != "finished") {
        sess$run(function() if (exists(".DB", envir = globalenv())) sxpdb::close_db(.DB))
        if (!quiet) {
            message("Stopped runner:", format(sess))
        }
        sess$close()
    }
}

#' @importFrom stringr str_replace fixed
#' @importFrom purrr quietly
#' @return either the result of running `fun(args)` or if `capture` is TRUE, a named list with
#    - error: chr        NA or the error / warning message that occurred (warnings are treated as errors)
#    - exit: int         0 or the exit that has crashed the underlying R session
#    - result: any       the result of calling fun with args
#' @export
runner_exec <- function(runner, fun, args, timeout_ms = 60 * 1000, capture = TRUE) {
    sess <- runner$sess
    if (sess$get_state() == "finished") {
        sess <- runner_create(lib_loc = runner$lib_loc)
        runner$sess <- sess
    }

    tryCatch(
        {
            state <- sess$get_state()

            if (state != "idle") {
                stop("R session is not ready: ", state)
            }

            if (capture) {
                fun2 <- fun
                fun <- generatr::capture_all(fun2)
            }

            sess$call(fun, args, package = TRUE)
            state <- sess$poll_process(timeout_ms)

            ret <- switch(state,
                ready = {
                    v <- sess$read()
                    if (v$code == 200 && is.null(v$error)) {
                        v$result
                    } else if (v$code == 200 && !is.null(v$error)) {
                        stop(v$error)
                    } else if (v$code == 501) {
                        stop(v$message)
                    } else {
                        stop("Call failed ", v$code, " (", v$message, ")")
                    }
                },
                timeout = stop("Timeout")
            )

            c(ret, list(exit = NA_integer_))
        },
        error = function(e) {
            # this should only happen if the underlying R session crashes
            # or timeout
            r <- list(
                error = e$message,
                exit = NA_integer_,
                result = NULL
            )

            state <- sess$get_state()

            if (state == "finished") {
                r$exit <- sess$get_exit_status()
            } else if (state == "busy") {
                sess$close(0L)
            }


            r
        }
    )
}

#' @export
print.runner <- function(x, ...) {
    sess <- x$sess
    print(paste("RUNNER: ", format(sess), " ", sess$get_status()))
}

#' @export
capture_all <- function(fun) {
    function(...) {
        tryCatch(
            {
                result <- fun(...)
                list(result = result, error = NA_character_)
            },
            error = function(e) list(result = NULL, error = e$message),
            warning = function(e) list(result = NULL, error = e$message),
            interrupt = function(e) stop("Terminated by user", call. = FALSE)
        )
    }
}
