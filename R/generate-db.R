library(sxpdb) # Obviously, for the DB stuff.
library(dplyr) # Tibbles > all.
library(purrr) # Makes R usable.
library(rlang) # Needed for metaprogramming (e.g., evaluating branch conditions)
library(callr) # For spinning up new R sessions to execute generated calls.

# Needs type system utils.

##
#   Constants
##

##
#   Setup the database.
##

# Make sure you save the result of this, it takes a while to compute.
# This works using the cran-4 DB.
#' @export
load_db <- function(path) {
    # Open the DB.
    DB <- open_db(path)

    # Build indices (this is required for sampling)
    build_indexes(DB)

    DB
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

get_random_value_db <- function(DB) {
    sample_val(DB)
}

# We probably want to use this one to get random value *indices*, then get the indexed values separately.
# This will be way more efficient.
get_random_idx_db <- function(DB) {
    sample_index(DB)
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
            # Generate the call.
            args_idx <- arg_seeds %>% map(function (lfp) {
                tryCatch((function() {
                    if (length(lfp) == 0) {
                        # Here, there haven't been any observed values for this parameter.
                        # We will just sample randomly until one succeeds.
                        sample_index(value_db)
                    } else {
                        seed_for_this_param <- sample(lfp, 1)
                        q <- query_from_value(seed_for_this_param);#no need to call close_query thanks to GC
                        idx <- NULL
                        j <- 1
                        relax_this_time <- sample(RELAX, (budget - i) / budget * length(RELAX))
                        while(is.null(idx) && j <= length(RELAX)) {
                          relax_query(q, relax_this_time)
                          idx <- sample_index(value_db, q)
                          relax_this_time <- unique(c(relax_this_time, RELAX[[j]]))
                          j <- j + 1
                        }
                        idx
                    }
                })(), error = function (e) {
                    print(e)
                })
            })
            
            cat(pkg, "::", fn_name, ": (", paste0(args_idx, collapse = ", "), ")\n", sep = "")
            
            args <- map(args_idx, function(idx) {
              tryCatch((function() {
                get_value_idx(value_db, idx)
              })(), error = function(e) {
                print(e)
              })
            })

            # TODO: Spin up a new process to execute this.
            return_pkg <- catchWarningsAndErrors(do.call(fn, args))
            # We can use callr::r to spin up a new R session.
            return_pkg <- catchWarningsAndErrors(callr::r(fn, args))

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

# Usage: fuzz_every_fn_in_pkg("stringr", value_db, origins_db, meta_db)
# Note: meta_db is currently unused.
#' @importFrom purrr map
#' @export
fuzz_every_fn_in_pkg <- function(pkg, value_db, origins_db, meta_db, budget = 10^3) {
    fn_names <- lsf.str(paste0("package:", pkg))

    funs <- fn_names %>% map(function(x) tryCatch(get(x), error = function(e) e))
    names(funs) <- fn_names
    
    results <- list()

    for (n in fn_names) {
        cat('Fuzzing', n, '...\n')
        results <- c(results, list(
            feedback_directed_call_generator_all_db(funs[[n]], pkg, n, value_db, origins_db, meta_db),
            error = function(e) e
        ))
    }

    names(results) <- fn_names

    results
}

####
# >>
# >> For exploring branch conditions.
# >

# This function should take some codeToEval, and the fn from which that code was drawn, and run it.
# This should be used in conjunction with the various code extractors from this file, which extract
# branch conditions from functions.
#
# TODO: Do we want to add the error handling here?
reconstructCodeExecution <- function(codeToEval, fn, value_db) {
    # First, get the environment the fn usually executes in.
    fnEnv <- rlang::fn_env(fn)

    # You can't just eval the expression in fnEnv, because the parameters won't be bound.
    # We should add bindings in fnEnv for all of the formals of fn.
    augmentedFnEnv <- unlockEnvironment(fnEnv)
    fnFormals <- names(formals(fn))
    names(fnFormals) <- fnFormals

    # Get random ids of DB values for each fn parameter.
    ids <- map(fnFormals, function(n) get_random_idx_db(value_db))

    map(fnFormals, function(n) augmentedFnEnv[[n]] <- get_value_idx(value_db, ids[[n]]))

    # Eval the codeToEval in the augmented environment.
    result <- eval(expr = codeToEval, envir = augmentedFnEnv)

    # Return a list with some important metadata.
    list( result = result,
          arg_ids = ids)
}

# Call this with:
# fn: the function you want to fuzz conditions for.
# value_db: the value_db to draw from.
#
# The idea is: 
fuzz_branch_conditions <- function(fn, value_db) {
    # TODO: Expand this function to get other conditions, too.
    # TODO: Atm, we're only doing switch.
    switch_conditions <- findBranchConditionsForFn_Switch(fn)
    if_conditions <- findBranchConditionsForFn_If(fn)

    conditions <- c(switch_conditions, if_conditions)

    # Now, we should fuzz each condition. 
    conditions %>% map(function(cond) {
        # We're going to give a budget of like 10k for this.
        budget <- 10^4

        run_results <- replicate(budget, tryCatch(
            reconstructCodeExecution(cond, fn, value_db),
            error = function(e) { 
                # Do nothing.
            }), simplify = FALSE) %>% discard(is.null)

        run_results$unique_results <- run_results %>% map(function(rp) rp[[1]]) %>% unique

        run_results
    })
}

unlockEnvironment <- function (env) {
  new.env(parent=env)
}