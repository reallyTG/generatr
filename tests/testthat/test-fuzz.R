test_that("fuzz one function", {

    db_path <- "db/str_dup"
    value_db <- sxpdb::open_db(db_path)
    origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble
    meta_db <- NULL

    runner <- runner_start()
    on.exit(runner_stop(runner))

    runner_fun <- create_fuzz_runner(db_path, runner)

    df <- feedback_directed_call_generator_all_db(
        stringr::str_dup,
        pkg_name = "stringr",
        fn_name = "str_dup",
        value_db = value_db,
        origins_db = origins_db,
        meta_db = meta_db,
        runner = runner_fun,
        budget = 42
    )

    str(df)
})
