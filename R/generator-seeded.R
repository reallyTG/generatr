#' @param seed a list of seeds
#' @export
create_seeded_args_generator <- function(value_db, seed) {
    state <- new.env(parent = emptyenv())
    class(state) <- "seeded_gen"

    state$i <- 0
    state$value_db <- value_db
    state$seed <- seed

    state
}

get_value.seeded_gen <- function(state, idx) {
    sxpdb::get_value_idx(state$value_db, idx)
}

remaining.seeded_gen <- function(state) {
    length(state$seed) - state$i
}

generate_args.seeded_gen <- function(state) {
    if (state$i < length(state$seed)) {
        state$i <- state$i + 1
        state$seed[[state$i]]
    } else {
        NULL
    }
}
