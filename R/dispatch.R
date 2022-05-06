#' @export
trace_dispatch_call <- function(fun, args, quote = TRUE,
                                environment = parent.frame()) {
    invisible(.Call(C_trace_dispatch_call, fun, args, environment))
}

#' @export
trace_dispatch_code <- function(code, quote = TRUE,
                           environment = parent.frame()) {
    if (quote) {
        code <- substitute(code)
    }

    invisible(.Call(C_trace_dispatch_code, code, environment))
}
