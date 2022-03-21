#' @export
generate_args <- function(state) {
    UseMethod("generate_args")
}

#' @export
successful_call <- function(state, args_idx) {
    UseMethod("successful_call")
}

#' @export
remaining <- function(state) {
    UseMethod("remaining")
}

#' @export
get_value <- function(state, idx) {
    UseMethod("get_value")
}

successful_call.default <- function(state, args_idx) {
    # nothing to do
}
