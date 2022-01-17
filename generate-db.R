
library(sxpdb)

# Make sure utilities are loaded.
source("generatr/generate-utils.R")

# Make sure the type system utilities are loaded.
source("generatr/generate-type-system-utils.R")

# We will use a rudimentary test generation approach for the sketch phase, so make sure that's loaded.
source("generatr/generate-randoop.R")

##
#   Setup the database.
##

# Make sure you save the result of this, it takes a while to compute.
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
    # For that reason, we will generate a value first, then call sample_similar.
    random_value_of_type <- get_random_value_for_type(type)

    sample_similar(DB, random_value_of_type, c("na", "length", "attributes", "vector", "ndims", "class"))
}

get_random_value_db_for_value <- function(val) {
    sample_similar(DB, val, c("na", "length", "attributes", "vector", "ndims", "class"))
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

test_function <- function(fn, budget = 10^3) {
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

# The old test_function, which just replicates generate_call_For_function.
#
# test_function <- function(fn, num_times = 100) {
#     replicate(num_times, generate_call_for_function(fn), simplify = FALSE)
# }

