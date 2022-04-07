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
    sess <- callr::r_session$new(wait = TRUE, options = callr::r_session_options(libpath = lib_loc))
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
#' @return a named list with
#    - error: chr        NA or the error message occurred
#    - exit: int         0 or the exit that has crashed the underlying R session
#    - messages: chr[]   the messages that occurred during the call
#    - output: chr       the output that occurred during the call
#    - result: any       the result of calling fun with args
#    - warnings: chr[]   the warnings that occurred during the call
#' @export
runner_exec <- function(runner, fun, args, timeout_ms = 60 * 1000) {
    sess <- runner$sess
    if (sess$get_state() == "finished") {
        sess <- runner_create(lib_loc = runner$lib_loc)
        runner$sess <- sess
    }

    tryCatch({
        state <- sess$get_state()

        if (state != "idle") {
            stop("R session is not ready: ", state)
        }

        sess$call(generatr::safely(fun), args, package = TRUE)
        state <- sess$poll_process(timeout_ms)

        ret <- switch(
            state,
            ready = {
                v <- sess$read()
                if (v$code == 200) {
                    v$result
                } else if (v$code == 501) {
                    stop(v$message)
                } else {
                    stop("Call failed ", v$code, " (", v$message, ")")
                }
            },
            timeout = stop("Timeout")
        )

        c(ret, list(exit = NA_integer_))
    }, error = function(e) {
        # this should only happen if the underlying R session crashes
        # or timeout
        r <- list(
            error = e$message,
            exit = NA_integer_,
            messages = NA_character_,
            output = NA_character_,
            result = NULL,
            warnings = NA_character_
        )

        state <- sess$get_state()

        if (state == "finished") {
            r$exit <- sess$get_exit_status()
        } else if (state == "busy") {
            sess$close(0L)
        }


        r
    })
}

#' @export
print.runner <- function(x, ...) {
    sess <- x$sess
    print(paste("RUNNER: ", format(sess), " ", sess$get_status()))
}

# The following are basically merge of purrr::safely and purrr::quietly 

#' @export
safely <- function(.f) {
  .f <- purrr:::as_mapper(.f)
  function(...) {
      v <- generatr::capture_all(.f(...))
      assign("..ANS", v, envir = globalenv())
      v
  }
}

#' @export
capture_all <- function(code) {
    warnings <- character()
    w_h <- function(w) {
        warnings <<- c(warnings, w$message)
        invokeRestart("muffleWarning")
    }

    messages <- character()
    m_h <- function(m) {
        messages <<- c(messages, m$message)
        invokeRestart("muffleMessage")
    }

    temp <- file()
    sink(temp)
    on.exit({
        sink()
        close(temp)
    })

    res <- tryCatch(
        {
            result <- withCallingHandlers(code, warning = w_h, message = m_h)
            list(result = result, error = NA_character_)
        },
        error = function(e) list(result = NULL, error = e$message),
        interrupt = function(e) stop("Terminated by user", call. = FALSE)
    )

    res$output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")
    res$warnings <- warnings
    res$messages <- messages

    res
}