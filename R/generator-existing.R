#' @importFrom purrr map map_int transpose
#' @importFrom dplyr filter select
#' @export
create_existing_args_generator <- function(pkg_name, fn_name, value_db, origins_db, lib_loc = .libPaths()) {
    seen_values <- dplyr::filter(origins_db, pkg_name == pkg_name, fun == fn_name)

    fn_formals <- withr::with_libpaths(
        lib_loc,
        formals(get(fn_name, envir = getNamespace(pkg_name), mode = "function"))
    )

    # the following is quite wrong:
    # the problem is that we keep just individual values
    # and not calls - we do not know which were used together
    # here is just something to put it together
    params <- names(fn_formals)

    # TODO: support ...
    params <- params[params != "..."]

    seed <- purrr::map(
                params,
                function(x) seen_values %>%
                    dplyr::filter(param == !!x) %>%
                    dplyr::select(id) %>%
                    unlist %>%
                    unname
            )

    seed_min <- min(purrr::map_int(seed, length))

    seed_t <- if (seed_min != 0) {
        seed <- purrr::map(seed, ~.[1:seed_min])

        # seed is a list where each element represents a parameter
        # what we need is that each element represents a call
        purrr::map(purrr::transpose(seed), as.integer)
    } else {
        list()
    }

    create_seeded_args_generator(value_db, seed_t)
}
