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

#' @export
quick_fuzz <- function(pkg_name, fn_name, db, budget, quiet = !interactive()) {
    runner <- runner_start(quiet = quiet)
    on.exit(runner_stop(runner, quiet = quiet))

    value_db <- if (is.character(db)) {
        sxpdb::open_db(db)
    } else {
        db
    }

    origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble

    generator <- create_fd_args_generator(pkg_name, fn_name,
                                          value_db = value_db, origins_db = origins_db, meta_db = NULL,
                                          budget = budget)

    runner_fun <- create_fuzz_runner(runner = runner, db_path = path_db(value_db))

    fuzz(pkg_name, fn_name, generator, runner_fun, quiet)
}

#' @importFrom purrr map map_dfr map_chr
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom progress progress_bar
#' @export
fuzz <- function(pkg_name, fn_name, generator, runner, 
                 quiet = !interactive(), 
                 get_type = contractr::infer_type) {
    # returns a list
    # - args_idx: int[]    - indicies to be used for the args
    # - error: chr         - the error from the function
    # - exit: int          - the exit code if the R session has crashed or 0
    # - messages: chr[]    - messages captured during the call
    # - output: chr        - output captured during the call
    # - result: any        - the function return value
    # - warnings: chr[]    - warnings captured during the call
    # - status: int        - 0 is OK, 1: warnings, 2: error, 3: crash, -1: generate_args failure, -2: runner failure
    # signature: chr       - the signatrue inferred from the call (using `get_type`)

    run_one <- function() {
        res <- list(
            args_idx = NA_integer_,
            error = NA_character_,
            exit = NA_integer_,
            messages = NA_character_,
            output = NA_character_,
            result = NULL,
            signature = NA_character_,
            status = 0L,
            warnings = NA_character_
        )
        class(res) <- "result"

        tryCatch({
            res$args_idx <- generate_args(generator)

            if (is.null(res$args_idx)) {
                return(NULL)
            }

            if (!is.integer(res$args_idx)) {
                stop("Generated value indices are not integers!")
            }
        }, error = function(e) {
            res$error <<- paste("fuzz-generate-args:", e$message)
            res$status <<- -1L
        })

        if (res$status != 0) {
            return(tibble::as_tibble(res))
        }

        tryCatch({
            r <- runner(pkg_name, fn_name, res$args_idx)

            res$error <- r$error
            res$messages <- r$messages
            res$output <- r$output
            res$exit <- r$exit
            res$warnings <- r$warnings

            if (!is.null(r$result)) {
                res$result <- r$result
            }
            if (any(!is.na(res$warnings))) {
                res$status <- 1L
            }
            if (!is.na(res$error)) {
                res$status <- 2L
            }
            if (!is.na(res$exit)) {
                res$status <- 3L
            }
        }, error = function(e) {
            res$error <<- e$message
            res$status <<- -2L
        })

        if (res$status == 0L) {
            successful_call(generator, res$args_idx)
        }

        if (res$status %in% c(0L, 1L)) {
            sig_args <- purrr::map_chr(res$args_idx, ~get_type(get_value(generator, .)))
            sig_args <- paste0(sig_args, collapse = ", ")
            res$signature <- paste0("(", sig_args, ") -> ", get_type(res$result))
        }

        tibble::as_tibble(res)
    }

    tick <- if (!quiet) {
        pb <- progress::progress_bar$new(
            format = paste0("  fuzzing ", pkg_name, ":::", fn_name, " [:bar] :current/:total (:percent) :elapsed"),
            total = remaining(generator), clear = FALSE, width = 80
        )
        function() pb$tick()
    } else {
        function() NULL
    }

    collected_results <- new.env(parent = emptyenv())
    i <- 1
    cont <- TRUE
    while (cont) {
        run <- run_one()
        if (is.null(run)) {
            cont <- FALSE
        } else {
            assign(as.character(i), run, envir = collected_results)
            i <- i + 1
            tick()
        }
    }

    df <- dplyr::bind_rows(as.list(collected_results))
    df
}

#' @param seed a list of seeds
create_seeded_args_generator <- function(value_db, seed) {
    state <- new.env(parent = emptyenv())
    class(state) <- "seeded_gen"

    state$i <- 0
    state$value_db <- value_db
    state$seed <- seed

    state
}

#' @importFrom purrr map
#' @importFrom dplyr filter select
create_fd_args_generator <- function(pkg_name, fn_name, value_db, origins_db, meta_db, budget) {
    state <- new.env(parent = emptyenv())
    class(state) <- "fd_gen"

    state$value_db <- value_db
    state$origins_db <- origins_db
    state$meta_db <- meta_db
    state$budget <- budget

    state$i <- 0

    # First, filter origins_db to get only things from the pkg::fn_name.
    state$seen_values <- origins_db %>% filter(pkg_name == pkg_name, fun == fn_name)

    fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")

    # Use these values as initial seeds for the value_db.
    state$arg_seeds <- formals(fn) %>%
        names %>%
        map(function(x) state$seen_values %>%
            filter(param == !!x) %>%
            select(id) %>%
            unlist %>%
            unname %>%
            map(function(y) sxpdb::get_value_idx(value_db, y)))

    names(state$arg_seeds) <- names(formals(fn))

    # Ok. Now, we want to gradually tighten up constraints.
    state$RELAX <- c("na", "length", "attributes", "vector", "ndims", "class", "type")

    state
}

#' @export
generate_args <- function(state) {
    UseMethod("generate_args")
}

#' @export
successful_call <- function(state, args_idx) {
    UseMethod("successful_call")
}

#' @export
remaining <- function(state) {
    UseMethod("remaining")
}

#' @export
get_value <- function(state, idx) {
    UseMethod("get_value")
}

get_value.fd_gen <- function(state, idx) {
    sxpdb::get_value_idx(state$value_db, idx)
}

get_value.seeded_gen <- function(state, idx) {
    sxpdb::get_value_idx(state$value_db, idx)
}

remaining.fd_gen <- function(state) {
    state$budget - state$i
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

#' @importFrom purrr map_int
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
                sample_index(state$value_db)
            } else {
                seed_for_this_param <- sample(lfp, 1)
                q <- query_from_value(seed_for_this_param);#no need to call close_query thanks to GC
                idx <- NULL
                # TODO: what to do it it is a NULL?
                j <- 1
                relax_this_time <- sample(state$RELAX, (state$budget - state$i) / state$budget * length(state$RELAX))
                while (is.null(idx) && j <= length(state$RELAX)) {
                    relax_query(q, relax_this_time)
                    idx <- sample_index(state$value_db, q)
                    relax_this_time <- unique(c(relax_this_time, state$RELAX[[j]]))
                    j <- j + 1
                }
           idx
           }
       }
    )

    args_idx
}

successful_call.seeded_gen <- function(state, args_idx) {
    # nothing to do
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

#' @export
as_tibble.result <- function(x, ...) {
    y <- x

    fix <- function(v) {
        if (length(v) == 0) {
            NA_character_
        } else if (length(v) == 1 && !is.na(v) && v == "") {
            NA_character_
        } else {
            paste0(v, collapse = ";")
        }
    }

    y$result <- NULL
    y$args_idx <- fix(x$args_idx)
    y$output <- trimws(y$output)
    y$output <- if (nchar(y$output) == 0) NA_character_ else y$output
    y$messages <- fix(x$messages)
    y$warnings <- fix(x$warnings)

    class(y) <- NULL
    tibble::as_tibble(y)
}

create_fuzz_runner <- function(db_path, runner) {
    # load db in the worker
    ret <- runner_exec(
        runner,
        function(db_path) {
            assign(".DB", sxpdb::open_db(db_path), envir = globalenv())
            0L
        },
        list(db_path)
    )

    if (ret$result != 0L) {
        stop("Unable to load DB in the runner: ", format(ret))
    }

    function(pkg_name, fn_name, args_idx) {
        runner_exec(
            runner,
            function(pkg_name, fn_name, args_idx, db_path) {
                if (!exists(".DB", envir = globalenv())) {
                    assign(".DB", sxpdb::open_db(db_path), envir = globalenv())
                }
                args <- lapply(args_idx, function(idx) sxpdb::get_value_idx(.DB, idx))
                fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")
                do.call(fn, args)
            },
            list(pkg_name, fn_name, args_idx, db_path)
        )
    }
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
