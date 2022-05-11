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
#' @importFrom rlang quo
#' @importFrom purrr map
process_multi_test_output_to_df <- function(output) {
    example_output <- output[[1]]

    nargs <- length(example_output$type)

    simplify_results_for_df <- function(res_line) {
        actual_args <- c(map(res_line$args, function(x) if (typeof(x) == "language") x else quo(x)), quo(!!res_line$ret))
        names(actual_args) <- paste0(names(res_line$type), "_v")
        c(res_line$type, actual_args, n_warn = length(res_line$warnings), n_err = length(res_line$errors), warnings = list(res_line$warnings), errors = list(res_line$errors))
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

# Create a value from a given t4r type.
# IMPORTANT NOTE: Due to how `...` works, this should be dealt with by the caller.
create_value_for_type <- function(type) {
    # First, check if it's a union type at the top level.
    tlu <- top_level_split(type, "|")
    if (length(tlu) > 1) {
        # Choose one of them randomly.
        do_me <- sample(1:length(tlu), 1)
        # Generate a type for it.
        create_value_for_type(tlu[do_me])
    } else {
        if (type == "integer") {
            1L
        } else if (type == "integer[]") {
            c(1L, 2L)
        } else if (type == "double") {
            1
        } else if (type == "double[]") {
            c(1, 2)
        } else if (type == "character") {
            "a"
        } else if (type == "character[]") {
            c("a", "b")
        } else if (type == "raw") {
            raw(1)
        } else if (type == "raw[]") {
            raw(2)
        } else if (type == "complex") {
            1i
        } else if (type == "complex[]") {
            c(1i, 2i)
        } else if (type == "logical") {
            TRUE
        } else if (type == "logical[]") {
            c(TRUE, FALSE)
        } else if (type == "^integer") {
            1L + NA
        } else if (type == "^integer[]") {
            c(1L, 1L + NA)
        } else if (type == "^double") {
            1 + NA
        } else if (type == "^double[]") {
            c(1, 2 + NA)
        } else if (type == "^character") {
            c("a", NA)[2]
        } else if (type == "^character[]") {
            c("a", c("a", NA)[2])
        } else if (type == "^complex") {
            1i + NA
        } else if (type == "^complex[]") {
            c(1i, 1i + NA)
        } else if (type == "^logical") {
            TRUE + NA
        } else if (type == "^logical[]") {
            c(TRUE, TRUE + NA)
        } else if (type == "null") {
            NULL
        } else if (substr(type, 1, 5) == "class") {
            # Sorry this LOC is gross.
            class_names <- substr(type, 7, nchar(type) - 1) %>% strsplit(', ') %>% unlist %>% unname %>% map(function(x) substr(x, 2, nchar(x) - 1)) %>% unlist
            r <- c(1)
            class(r) <- class_names
            r
        } else if (substr(type, 1, 4) == "list") {
            # Let's make, like, 5 values of various types in the list.
            inner_type <- substr(type, 6, nchar(type) - 1)
            replicate(5, create_value_for_type(inner_type), simplify = FALSE)
        }
        
        # TODO: Are there other types?
    }
}

top_level_split <- function(t, split, just_one=TRUE, init_num_brackets = 0, init_num_chevrons = 0) {
    if (!just_one) {
        return(map(t, function(ti) top_level_split(ti, split, just_one=TRUE)))
    }
    
    if (is.na(t))
        return(NA)

    num_brackets <- init_num_brackets
    num_chevrons <- init_num_chevrons

    parsing_name <- FALSE

    inds <- c(0)
    for (i in 1:nchar(t)) {
        the_char <- substr(t, i, i)

        if (the_char == "`")
            parsing_name <- !parsing_name
        
        if(!parsing_name) {
            if (the_char == "[") {
                num_brackets <- num_brackets + 1
            } else if (the_char == "<") {
                num_chevrons <- num_chevrons + 1
            } else if (the_char == "]") {
                num_brackets <- num_brackets - 1
            } else if (the_char == ">") {
                num_chevrons <- num_chevrons - 1
            } else if (the_char == split && num_brackets == 0 && num_chevrons == 0) {
                inds <- c(inds, i)
            }
        }
    }

    inds <- c(inds, nchar(t))

    # inds contains 0, and the indices of split
    if (length(inds) == 1) {
        return(list()) # no tags
    }
    r <- c()
    for (i in 1:(length(inds) - 1)) {
        if (i == length(inds) - 1) 
            r <- c(r, str_trim(substr(t, inds[i] + 1, inds[i+1])))
        else
            r <- c(r, str_trim(substr(t, inds[i] + 1, inds[i+1] - 1)))
    }

    r
}

# Make sure to slice the df before going in here.
# e.g., df %>% slice(1) %>% get_arg_values_for_slice
get_arg_values_for_slice <- function(df) {
    df %>% select(ends_with("_v")) %>% unlist %>% map(eval_tidy)
}

forbidden_types <- c("environment", "closure")
metadata_from_value <- function(v) {
  typeOfV <- typeof(v)

  list(
    sexptype = typeOfV,
    classes = class(v),
    length = length(v),
    n_attribs = length(attributes(v)),
    n_dims = length(dim(v)),
    n_rows = if (!is.null(dim(v))) nrow(v) else 0L,
    has_na = if (typeOfV %in% forbidden_types) FALSE else anyNA(v)
  )
}

create_db <- function(...) {
    db_path <- tempfile()
    db <- sxpdb::open_db(db_path, mode = T)
    values <- list(...)
    for (x in values) {
        sxpdb::add_val(db, x)
    }
    sxpdb::close_db(db)

    sxpdb::open_db(db_path)
}

