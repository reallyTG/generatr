library(sxpdb)
library(dplyr)
library(purrr)
library(rlang)

##
#   Constants
##
FRACTION_SPENT_EXPLORING <- 0.1

##
#   Setup the database.
##

# Make sure you save the result of this, it takes a while to compute.
# This works using the CRAN-100 DB.
#' @export
load_db <- function(path) {
    # Open the DB.
    DB <- open_db(path)

    # Build indices (this is required for sampling)
    build_indexes(DB)

    DB
}

finish <- function() {
    close_db(DB)
}

catchWarningsAndErrors <- function(expr) {
    myWarnings <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w))
        invokeRestart("muffleWarning")
    }
    myErrors <- NULL
    eHandler <- function(e) {
        myErrors <<- c(myErrors, list(e))
        NULL
    }
    val <- withCallingHandlers(tryCatch(expr, error = eHandler), warning = wHandler)
    list(value = val, warnings = myWarnings, errors = myErrors)
}

get_random_value_db <- function() {
    sample_val(DB)
}

# As of mid-Jan, you can relax on:
# c("na", "length", "attributes", "vector", "ndims", "class", "type")
get_random_value_db_for_type <- function(type) {
    # Currently, the API requires a value to be compared against, and not a type.
    # For that reason, we will generate a value first, then call get_random_value_db_for_value.
    random_value_of_type <- create_value_for_type(type)

    get_random_value_db_for_value(random_value_of_type)
}

# This: `sample_similar(DB, val, c("length", "attributes", "vector", "ndims"))`
# Means: keep type, class, and na the same, anything else can change.
#
# The things we can relax on are: c("na", "length", "attributes", "vector", "ndims", "class", "type").
# By default, we don't relax on type. We can probably change that, as we direct the type exploration.
# TODO: Check new class API, and try this function with a function that has a class.
get_random_value_db_for_value <- function(val) {
    if (exists("DB")) {
        sample_similar(DB, val, c("length", "attributes", "vector", "ndims"))
    } else {
        val
    }
}

generate_call_for_function <- function(fn) {
    fn_params <- formals(fn)
    num_params <- length(fn_params)
    call_res <- generate_call_for_function_inner(fn, num_params)

    call_res
}

generate_call_for_function_inner <- function(fn, num_params) {
    args <- replicate(num_params, get_random_value_db(), simplify = FALSE)
    return_pkg <- catchWarningsAndErrors(do.call(fn, args))

        list(args = args, 
        ret = return_pkg$value, 
        type = get_type_for_args_and_ret(args, return_pkg$value), 
        warnings = return_pkg$warnings, 
        errors = return_pkg$errors)
}

sketch_type <- function(fn, budget) {
    # Here, we just want to pepper the function with random stuff. 
    # A smarter thing to do might be to make a matrix: nrow = # parameters, ncol = # high-level types
    # and ensure that we have good coverage. Even so, there's interplay between the parameters.

    # First attempt: let's just generate random calls.
    unprocessed_results <- replicate(budget, generate_call_for_function_simple(fn), simplify = FALSE)

    process_multi_test_output_to_df(unprocessed_results)
}

generate_call_for_function_with_like_values <- function(fn, actual_values, value_gen_fn) {
    args <- map(actual_values, value_gen_fn)
    return_pkg <- catchWarningsAndErrors(do.call(fn, args))

    list(args = args, 
        ret = return_pkg$value, 
        type = get_type_for_args_and_ret(args, return_pkg$value), 
        warnings = return_pkg$warnings, 
        errors = return_pkg$errors)
}

generate_call_for_function_from_passing_tests <- function(fn, pasing_calls, value_gen_fn) {
    fn_params <- formals(fn)
    num_params <- length(fn_params)
    random_passing_test <- passing_calls %>% slice(sample(nrow(passing_calls), 1))
    quoted_values <- random_passing_test %>% select(contains("_v")) %>% select(contains("arg"))
    actual_values <- quoted_values %>% unlist %>% unname %>% map(eval_tidy)

    call_res <- generate_call_for_function_with_like_values(fn, actual_values, value_gen_fn)

    call_res
}

fill_in_passing_types <- function(fn, passing_calls, budget, value_generation_method = "DB") {
    # For this, we will use the database.
    value_gen_fn <- get_random_value_for_value
    if (value_generation_method == "DB")
        value_gen_fn <- get_random_value_db_for_value

    # value_gen_fn should always take a value.
    unprocessed_results <- replicate(budget, generate_call_for_function_from_passing_tests(fn, pasing_calls, value_gen_fn), simplify = FALSE)

    process_multi_test_output_to_df(unprocessed_results)
}

# We might not need this anymore; moving logic to feedback_directed_call_generator
generate_call_for_function_from_type <- function(fn, parsed_type) {

    args <- parsed_type$parameter_types %>% 
                map(create_value_for_type) %>%
                map(get_random_value_db_for_value)

    return_pkg <- catchWarningsAndErrors(do.call(fn, args))

    list(args = args, 
        ret = return_pkg$value, 
        type = get_type_for_args_and_ret(args, return_pkg$value), 
        warnings = return_pkg$warnings, 
        errors = return_pkg$errors)
}

feedback_directed_call_generator_from_type <- function(fn, budget, type) {
    # First, parse type.
    parsed_type <- parse_type(type)

    parameter_types <- parsed_type$parameter_types

    # Idea: Figure out valid and invalid call signatures.
    # First, how much time are we going to spend generating initial signatures?
    explore_budget <- budget * FRACTION_SPENT_EXPLORING
    budget <- budget - explore_budget

    # Generate initial signatures.
    processed_results <- process_multi_test_output_to_df(replicate(explore_budget, generate_call_for_function_from_type(fn, parsed_type), simplify = FALSE))

    successful_signatures <- processed_results %>% filter(n_err + n_warn == 0)
    unsuccessful_signatures <- processed_results %>% filter(n_err + n_warn > 0)

    group_variables <- syms(c(paste0("arg", 1:length(formals(fn))))) # , "ret"))

    unique_successful_signatures <- successful_signatures %>%
                                        group_by(!!!group_variables) %>%
                                        summarize %>%
                                        ungroup

    unique_unsuccessful_signatures <- unsuccessful_signatures %>%
                                        group_by(!!!group_variables) %>%
                                        summarize %>%
                                        ungroup

    uss <- 1:nrow(unique_successful_signatures) %>% map(function (x) unique_successful_signatures %>% slice(x) %>% unlist %>% unname)
    uus <- 1:nrow(unique_unsuccessful_signatures) %>% map(function (x) unique_unsuccessful_signatures %>% slice(x) %>% unlist %>% unname)

    results_from_fuzzing_successful_signatures <- uss %>% map(function (sig) fuzz_fn_with_signature(fn, sig))

    # processed_results
    unique_unsuccessful_signatures
}

#' @importFrom purrr map
#' @importFrom dplyr filter select
#' @export
feedback_directed_call_generator_all_db <- function(fn, pkg, fn_name, value_db, origins_db, meta_db, budget = 10^3) {
    # First, filter origins_db to get only things from the pkg::fn_name.
    seen_values <- origins_db %>% filter(pkg == pkg, fun == fn_name)

    # Use these values as initial seeds for the value_db.
    arg_seeds <- formals(fn) %>% 
        names %>% 
        map(function(x) seen_values %>% 
            filter(param == !!x) %>% 
            select(id) %>% 
            unlist %>% 
            unname %>% 
            map(function(y) get_value_idx(value_db, y)))

    names(arg_seeds) <- names(formals(fn))

    # Ok. Now, we want to gradually tighten up constraints.
    RELAX <- c("na", "length", "attributes", "vector", "ndims", "class", "type")

    collected_results <- list()

    # Dumb loop, make this better.
    for (i in 1:budget) {
        tryCatch((function () {
            relax_this_time <- sample(RELAX, (budget - i) / budget * length(RELAX))

            # Generate the call.
            args <- arg_seeds %>% map(function (lfp) {
                tryCatch((function() {
                    if (length(lfp) == 0) {
                        # Here, there haven't been any observed values for this parameter.
                        # We will just sample randomly until one succeeds.
                        sample_val(value_db)
                    } else {
                        seed_for_this_param <- sample(lfp, 1)
                        sample_similar(value_db, seed_for_this_param, relax_this_time)
                    }
                })(), error = function (e) {
                    print(e)
                })
            })

            return_pkg <- catchWarningsAndErrors(do.call(fn, args))

            results <- list(args = args, 
                ret = return_pkg$value, 
                type = get_type_for_args_and_ret(args, return_pkg$value), 
                warnings = return_pkg$warnings, 
                errors = return_pkg$errors)

            collected_results <<- c(collected_results, list(results))

            if (length(results$warnings) + length(results$errors) == 0) {
                # Call was probably successful.
                # Save the arguments as future seeds.
                for (n in names(args)) {
                    arg_seeds[[n]] <<- union(arg_seeds[[n]], list(args[[n]]))
                }
            }
        })(), error = function (e) {
            # Do nothing.
            print('Error creating a call:')
            print(e)
        })
    }

    process_multi_test_output_to_df(collected_results)
}

# Note: Relaxation parameters for DB: c("na", "length", "attributes", "type", "vector", "ndims", "class")
fill_type_from_type <- function(fn, budget = 10^3, init_type, value_generation_method = "DB") {
    # For this, we will use the database.
    # TODO: Right now, we're just always gonna use the database.
    value_gen_fn <- get_random_value_for_value
    if (value_generation_method == "DB")
        value_gen_fn <- get_random_value_db_for_value

    feedback_directed_call_generator_from_type(fn, budget, init_type)

    # value_gen_fn should always take a value.
    # unprocessed_results <- replicate(budget, generate_call_for_function_from_type(fn, parse_type(init_type)), simplify = FALSE)

    # process_multi_test_output_to_df(unprocessed_results)
}

test_function <- function(fn, budget = 10^3, init_type = "") {

    # Did the user supply a type?
    if (init_type == "") {
        # If yes, skip the sketch phase and use the type.
        fill_type_from_type(fn, budget, parse_type(init_type))
    } else {
        # If not...
        # First, let's sketch the type of the function. 
        # Q1: How much of the budget do we spend on this?
        # ??? Will use 1/4 of the budget on this.
        sketch_results <- sketch_type(fn, budget = budget / 4)

        passing_calls <- sketch_results %>% filter(n_err == 0 & n_warn == 0)
        failing_calls <- sketch_results %>% filter(n_err != 0 | n_warn != 0)

        # Now, we have discovered a number of passing and failing calls (in their respective DFs).
        # We will generate more calls that have type signatures similar to the existing passing calls
        # to ensure that the signatures are valid.
        # ??? We will use 1/4 of the budget for this again.
        fill_results <- fill_in_passing_types(fn, passing_calls, budget = budget / 4)

        passing_fill_results <- fill_results %>% filter(n_err == 0 & n_warn == 0)
        failing_fill_results <- fill_results %>% filter(n_err != 0 | n_warn != 0)
    }
}

# Usage: fuzz_every_fn_in_pkg("stringr", value_db, origins_db, meta_db)
# Note: meta_db is currently unused.
#' @importFrom purrr map
#' @export
fuzz_every_fn_in_pkg <- function(pkg, value_db, origins_db, meta_db, budget = 10^3) {
    fn_names <- lsf.str(paste0("package:", pkg))
    
    # DEBUG: TODO
    # fn_names <- head(fn_names)

    funs <- fn_names %>% map(function(x) tryCatch(get(x), error = function(e) e))
    names(funs) <- fn_names
    
    results <- list()

    for (n in fn_names) {
        cat('Fuzzing', n, '...\n')
        # results <- c(results, list(tryCatch(
        #     feedback_directed_call_generator_all_db(funs[[n]], pkg, n, value_db, origins_db, meta_db),
        #     error = function(e) e
        # )))
        results <- c(results, list(
            feedback_directed_call_generator_all_db(funs[[n]], pkg, n, value_db, origins_db, meta_db),
            error = function(e) e
        ))
    }

    names(results) <- fn_names

    results
}
