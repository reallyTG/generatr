test_that("fuzz one function", {
  value_db <- sxpdb::open_db(DB_PATH)
  origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble
  meta_db <- NULL

  runner <- runner_start()
  on.exit(runner_stop(runner))

  runner_fun <- function(pkg_name, fn_name, args_idx) {
    fn <- get(fn_name, envir = getNamespace(pkg_name), mode = "function")
    # TODO move this into the runner itself
    args <- map(args_idx, ~sxpdb::get_value_idx(value_db, .))
    runner_exec(runner, fn, args)
  }

  feedback_directed_call_generator_all_db(
      stringr::str_dup,
      pkg_name = "stringr",
      fn_name = "str_dup",
      value_db = value_db,
      origins_db = origins_db,
      meta_db = meta_db,
      runner = runner_fun,
      budget = 10
  )
})
