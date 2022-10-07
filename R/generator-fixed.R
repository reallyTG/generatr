#' @param seed a list of seeds
#' @export
create_fixed_args_generator <- function(db, seed) {
    state <- new.env(parent = emptyenv())
    class(state) <- "fixed_gen"

    state$i <- 0
    state$seed <- seed
    state$value_db <- db

    state
}

get_value.fixed_gen <- function(state, idx) {
    sxpdb::get_value_idx(state$value_db, idx)
}

remaining.fixed_gen <- function(state) {
    length(state$seed) - state$i
}

generate_args.fixed_gen <- function(state) {
    if (state$i < length(state$seed)) {
        state$i <- state$i + 1
        state$seed[[state$i]]
    } else {
        NULL
    }
}
