#' @importFrom purrr map map_int transpose
#' @importFrom dplyr filter
#' @export
create_existing_args_generator <- function(pkg_name, fn_name, value_db, origins_db) {
    seen_values <- filter(origins_db, pkg_name == pkg_name, fun == fn_name)
    fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")

    # the following is quite wrong:
    # the problem is that we keep just individual values
    # and not calls - we do not know which were used together
    # here is just something to put it together
    seed <- formals(fn) %>%
        names %>%
        purrr::map(function(x) seen_values %>%
            filter(param == !!x) %>%
            select(id) %>%
            unlist %>%
            unname)

    seed_min <- min(purrr::map_int(seed, length))
    seed <- purrr::map(seed, ~.[1:seed_min])

    # seed is a list where each element represents a parameter
    # what we need is that each element represents a call
    seed_t <- purrr::map(purrr::transpose(seed), as.integer)

    create_seeded_args_generator(value_db, seed_t)
}
