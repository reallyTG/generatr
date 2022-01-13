
source("generatr/generate-utils.R")

## To Do
#
# TODO 1 Improve this to be an actual implementation of randoop, incl. creating sequences and saving them
# TODO 1 (even though it's probably not relevant)
#
#####################################################

## Ideas for a smarter way to generate calls.
#
# 1. Fuzz with NAs, NULLs;
# 2. Fuzz with random classes, attributes;
# 3. Fuzz with lengths;
# 4. Fuzz by changing attribute values;
# 5. For character arguments, try (column) names from other arguments;
# 6.
#
######################################################

##
#  Random primitives.
##
R_INTS <- c(-2L, -1L, 0L, 1L, 2L)
R_DBLS <- c(-2, -1.5, -1, -0.5, 0, 0.1, 1, 1.5, 2)
R_LGLS <- c(TRUE, FALSE)
R_CLXS <- c(R_INTS, R_INTS + 1i)
R_CHRS <- c("a", "b", "c", "d", "e")
R_RAWS <- c(as.raw(0), as.raw(1), as.raw(2), as.raw(3), as.raw(4)) 

LIST_OF_PREVIOUSLY_SEEN_VALUES <- list(NULL)

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

get_random_int <- function(n) {
    sample(R_INTS, n, TRUE)
}

get_random_dbl <- function(n) {
    sample(R_DBLS, n, TRUE)
}

get_random_lgl <- function(n) {
    sample(R_LGLS, n, TRUE)
}

get_random_clx <- function(n) {
    sample(R_CLXS, n, TRUE)
}

get_random_chr <- function(n) {
    sample(R_CHRS, n, TRUE)
}

get_random_raw <- function(n) {
    sample(R_RAWS, n, TRUE)
}

get_random_list <- function(n) {
    replicate(n, get_random_value(), simplify = FALSE)
}

get_random_matrix <- function(n) {
    # They'll all get coerced to a reasonable type :-)
    replicate(sample(1:5, 1), get_random_vector(n))
}

get_random_vector <- function(n) {
    which <- sample(c("int", "dbl", "clx", "raw", "chr", "lgl"), 1)

    if (which == "int") {
        get_random_int(n)
    } else if (which == "dbl") {
        get_random_dbl(n)
    } else if (which == "clx") {
        get_random_clx(n)
    } else if (which == "raw") {
        get_random_raw(n)
    } else if (which == "chr") {
        get_random_chr(n)
    } else if (which == "lgl") {
        get_random_lgl(n)
    }
}

get_random_df <- function(cols, n) {
    if (n == 1)
        as.data.frame(t(replicate(cols, get_random_vector(n))))
    else
        as.data.frame(replicate(cols, get_random_vector(n)))
}

get_random_value <- function() {
    which <- sample(c("int", "dbl", "clx", "raw", "chr", "lgl", "list", "df", "previous", "matrix"), 1)
    how_many <- sample(1:5, 1)

    if (which == "int") {
        get_random_int(how_many)
    } else if (which == "dbl") {
        get_random_dbl(how_many)
    } else if (which == "clx") {
        get_random_clx(how_many)
    } else if (which == "raw") {
        get_random_raw(how_many)
    } else if (which == "chr") {
        get_random_chr(how_many)
    } else if (which == "lgl") {
        get_random_lgl(how_many)
    } else if (which == "list") {
        get_random_list(how_many)
    } else if (which == "df") {
        get_random_df(sample(1:5, 1), how_many)
    } else if (which == "previous") {
        # Get a value seen previously.
        LIST_OF_PREVIOUSLY_SEEN_VALUES[[sample(1:length(LIST_OF_PREVIOUSLY_SEEN_VALUES), 1)]]
    } else if (which == "matrix") {
        get_random_matrix(how_many)
    }
}

generate_call_for_function <- function(fn) {
    fn_params <- formals(fn)
    num_params <- length(fn_params)
    call_res <- generate_call_for_function_inner(fn, num_params)

    if (is.null(call_res$warnings) & is.null(call_res$errors)) {
        if (typeof(call_res$ret) != "list")
            LIST_OF_PREVIOUSLY_SEEN_VALUES <- c(LIST_OF_PREVIOUSLY_SEEN_VALUES, call_res$ret)
        else
            LIST_OF_PREVIOUSLY_SEEN_VALUES <- c(LIST_OF_PREVIOUSLY_SEEN_VALUES, list(call_res$ret))
    }

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

test_function <- function(fn, num_times = 100, .infer = FALSE) {
    res <- replicate(num_times, generate_call_for_function(fn), simplify = FALSE)

    if (.infer) {
        # Collect all successful runs.
        good_calls <- Filter(function(c) is.null(c$warnings) & is.null(c$errors), res)

        res$infered_type <- consolidate_types(Map(function(gc) gc$type, good_calls))
    }

    res
}

get_stats_for_run <- function(listOfCalls) {
    n_warn <- length(Filter(function(c) !is.null(c$warnings), listOfCalls))
    n_err <- length(Filter(function(c) !is.null(c$errors), listOfCalls))
    n_warn_and_err <- length(Filter(function(c) !is.null(c$errors) & !is.null(c$warnings), listOfCalls))

    list(ok = length(listOfCalls) - n_warn - n_err, warnings = n_warn, errors = n_err, warnings_and_errors = n_warn_and_err)
}

# TODO: Test with some real functions. E.g., stuff in `stringr`. 

