#' @importFrom purrr map
#' @importFrom dplyr filter select
#' @export
create_fd_args_generator <- function(pkg_name, fn_name, value_db, origins_db, meta_db, budget, lib_loc = .libPaths()) {
    state <- new.env(parent = emptyenv())
    class(state) <- "fd_gen"

    state$value_db <- value_db
    state$origins_db <- origins_db
    state$meta_db <- meta_db
    state$budget <- budget

    state$i <- 0

    # First, filter origins_db to get only things from the pkg::fn_name.
    state$seen_values <- origins_db %>% filter(pkg_name == pkg_name, fun == fn_name)

    fn_formals <- withr::with_libpaths(
        lib_loc,
        formals(get(fn_name, envir = getNamespace(pkg_name), mode = "function"))
    )

    # Use these values as initial seeds for the value_db.
    state$arg_seeds <- fn_formals %>%
        names %>%
        map(function(x) state$seen_values %>%
            filter(param == !!x) %>%
            select(id) %>%
            unlist %>%
            unname %>%
            map(function(y) sxpdb::get_value_idx(value_db, y)))

    names(state$arg_seeds) <- names(fn_formals)

    # Ok. Now, we want to gradually tighten up constraints.
    state$RELAX <- c("na", "length", "attributes", "vector", "ndims", "class", "type")

    state
}

get_value.fd_gen <- function(state, idx) {
    sxpdb::get_value_idx(state$value_db, idx)
}

remaining.fd_gen <- function(state) {
    state$budget - state$i
}

#' @importFrom purrr map_int
#' @importFrom sxpdb relax_query sample_index query_from_value
generate_args.fd_gen <- function(state) {
    if (state$i >= state$budget) {
        return(NULL)
    }

    state$i <- state$i + 1

    # TODO: keep the set of idx that have been alreay tried

    args_idx <- purrr::map_int(
        state$arg_seeds,
        function(lfp) {
            if (length(lfp) == 0) {
                # Here, there haven't been any observed values for this parameter.
                # We will just sample randomly until one succeeds.
                sxpdb::sample_index(state$value_db)
            } else {
                seed_for_this_param <- sample(lfp, 1)
                q <- sxpdb::query_from_value(seed_for_this_param);#no need to call close_query thanks to GC
                idx <- NULL
                # TODO: what to do it it is a NULL?
                j <- 1
                relax_this_time <- sample(state$RELAX, (state$budget - state$i) / state$budget * length(state$RELAX))
                while (is.null(idx) && j <= length(state$RELAX)) {
                    sxpdb::relax_query(q, relax_this_time)
                    idx <- sxpdb::sample_index(state$value_db, q)
                    relax_this_time <- unique(c(relax_this_time, state$RELAX[[j]]))
                    j <- j + 1
                }
                if(is.null(idx)) { # we should have relaxed enough anyway
                  q <- sxpdb::query_from_value(seed_for_this_param)
                  sxpdb::relax_query(q, "keep_type")
                }
                if(is.null(idx)) { # we should have relaxed enough anyway
                  idx <- sxpdb::sample_index(state$value_db)
                }
           idx
           }
       }
    )

    args_idx
}

#' @importFrom purrr map
#' @importFrom sxpdb get_value_idx
successful_call.fd_gen <- function(state, args_idx) {
    # Call was probably successful.
    # Save the arguments as future seeds.
    for (n in names(args_idx)) {
        args <- purrr::map(args_idx, ~ sxpdb::get_value_idx(state$value_db, .))
        state$arg_seeds[[n]] <- union(state$arg_seeds[[n]], list(args[[n]]))
    }
}
