#' @export
runner_start <- function(quiet = TRUE) {
    runner <- new.env(parent = emptyenv())
    runner$sess <- runner_create(quiet)
    class(runner) <- "runner"
    runner
}

#' @importFrom callr r_session
runner_create <- function(quiet = TRUE) {
    sess <- callr::r_session$new(wait = TRUE)
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
        if (!quiet) {
            message("Stopped runner:", format(sess))
        }
        sess$close()
    }
}

#' @importFrom purrr quietly
#' @return a named list with
#    - error: chr        NA or the error message occurred
#    - exit: int         0 or the exit that has crashed the underlying R session
#    - messages: chr[]   the messages that occurred during the call
#    - output: chr       the output that occurred during the call
#    - result: any       the result of calling fun with args
#    - warnings: chr[]   the warnings that occurred during the call
#' @export
runner_exec <- function(runner, fun, args) {
    sess <- runner$sess
    if (sess$get_state() == "finsihed") {
        sess <- runner_create()
        runner$sess <- sess
    }

    tryCatch({
        ret <- sess$run(purrr::quietly(fun), args, package = TRUE)
        c(ret, list(error = NA_character_, exit = 0L))
    }, error = function(e) {
        r <- list(
            error = e$message,
            exit = 0L,
            messages = NA_character_,
            output = NA_character_,
            result = NULL,
            warnings = NA_character_
        )

        if (sess$get_state() == "finished") {
            r$exit <- sess$get_exit_status()
        }

        r
    })
}

#' @export
print.runner <- function(x, ...) {
    sess <- x$sess
    print(paste("RUNNER: ", format(sess), " ", sess$get_status()))
}
