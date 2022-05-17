#' @export
trace_dispatch_call <- function(fun, args, quote = TRUE,
                                environment = parent.frame()) {
    invisible(.Call(C_trace_dispatch_call, fun, args, environment))
}
