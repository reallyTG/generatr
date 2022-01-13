
library(sxpdb)

source("generatr/generate-utils.R")

##
#   Setup the database.
##

DB <- NULL

load_db <- function(path) {
    DB <<- open_db(path)
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

get_random_value <- function() {
    sample_val(DB)
}

generate_call_for_function <- function(fn) {
    fn_params <- formals(fn)
    num_params <- length(fn_params)
    call_res <- generate_call_for_function_inner(fn, num_params)

    # if (is.null(call_res$warnings) & is.null(call_res$errors)) {
    #     if (typeof(call_res$ret) != "list")
    #         LIST_OF_PREVIOUSLY_SEEN_VALUES <- c(LIST_OF_PREVIOUSLY_SEEN_VALUES, call_res$ret)
    #     else
    #         LIST_OF_PREVIOUSLY_SEEN_VALUES <- c(LIST_OF_PREVIOUSLY_SEEN_VALUES, list(call_res$ret))
    # }

    call_res
}

generate_call_for_function_inner <- function(fn, num_params) {
    args <- replicate(num_params, get_random_value(), simplify = FALSE)
    return_pkg <- catchWarningsAndErrors(do.call(fn, args))

        list(args = args, 
        ret = return_pkg$value, 
        type = get_type_for_args_and_ret(args, return_pkg$value), 
        warnings = return_pkg$warnings, 
        errors = return_pkg$errors)
}

test_function <- function(fn, num_times = 100) {
    replicate(num_times, generate_call_for_function(fn), simplify = FALSE)
}

get_stats_for_run <- function(listOfCalls) {
    n_warn <- length(Filter(function(c) !is.null(c$warnings), listOfCalls))
    n_err <- length(Filter(function(c) !is.null(c$errors), listOfCalls))
    n_warn_and_err <- length(Filter(function(c) !is.null(c$errors) & !is.null(c$warnings), listOfCalls))

    list(ok = length(listOfCalls) - n_warn - n_err, warnings = n_warn, errors = n_err, warnings_and_errors = n_warn_and_err)
}

# TODO: Test with some real functions. E.g., stuff in `stringr`. 

