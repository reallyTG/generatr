
library(tidyverse)
library(rlang)

get_stats_for_run <- function(listOfCalls) {
    n_warn <- length(Filter(function(c) !is.null(c$warnings), listOfCalls))
    n_err <- length(Filter(function(c) !is.null(c$errors), listOfCalls))
    n_warn_and_err <- length(Filter(function(c) !is.null(c$errors) & !is.null(c$warnings), listOfCalls))

    list(ok = length(listOfCalls) - n_warn - n_err, warnings = n_warn, errors = n_err, warnings_and_errors = n_warn_and_err)
}

#
# Take the output of the type sketch phase and turn it into a useful data frame.
#
process_multi_test_output_to_df <- function(output) {
    example_output <- output[[1]]

    nargs <- length(example_output$type)

    simplify_results_for_df <- function(res_line) {
        actual_args <- c(map(res_line$args, function(x) if (typeof(x) == "language") x else quo(x)), quo(!!res_line$ret))
        names(actual_args) <- paste0(names(res_line$type), "_v")
        c(res_line$type, actual_args, n_warn = length(res_line$warnings), n_err = length(res_line$errors))
    }

    ready_for_df <- map(output, simplify_results_for_df)

    df <- as.data.frame(do.call(rbind, ready_for_df)) %>% as_tibble

    format_df <- function(df) {
        df$n_warn <- as.integer(df$n_warn)
        df$n_err <- as.integer(df$n_err)

        cols_to_change <- names(example_output$type)

        map(cols_to_change, function(col_name) {
            df[,col_name] <<- unlist(df[,col_name])
        })

        df
    }

    df <- format_df(df)

    df
}

#
# From the df generated from the output of the Sketch phase, try to figure out
# which parameter types are stable. 
#
discover_stable_parameter_types <- function(sketch_df) {
    # A paremeter type is _stable_ if ... Is this even important?
}