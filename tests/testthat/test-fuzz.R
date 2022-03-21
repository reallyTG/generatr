test_that("fuzz one function", {

    pkg_name <- "stringr"
    fn_name <- "str_dup"
    db_path <- "db/str_dup"

    seed <- list(
        c(7L, 8L),
        c(0L, 0L),
        c(5L, 6L),
        c(5L, 9L),
        c(4L, 5L),
        c(5L, 10L),
        c(1L, 11L),
        c(5L, 7L),
        c(10L, 9L),
        c(5L, 7L),
        c(9L, 8L),
        c(7L, 1L),
        c(5L, 7L),
        c(11L, 1L),
        c(7L, 5L),
        c(10L, 7L),
        c(11L, 5L),
        c(5L, 7L),
        c(7L, 5L),
        c(7L, 6L),
        c(5L, 11L),
        c(11L, 7L),
        c(7L, 6L),
        c(5L, 5L),
        c(5L, 6L),
        c(9L, 7L),
        c(6L, 5L),
        c(5L, 5L),
        c(5L, 5L),
        c(7L, 7L),
        c(5L, 5L),
        c(7L, 5L),
        c(7L, 5L),
        c(7L, 7L),
        c(7L, 7L),
        c(7L, 7L),
        c(5L, 7L),
        c(5L, 7L),
        c(5L, 7L),
        c(7L, 7L),
        c(7L, 5L),
        c(5L, 7L)
    )

    runner <- runner_start()
    on.exit(runner_stop(runner))
    runner_fun <- create_fuzz_runner(db_path, runner)

    value_db <- sxpdb::open_db(db_path)
    origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble
    meta_db <- NULL

    # the following uses the feedback directed (fd) generator
    # budget <- 42
    # generator <- create_fd_args_generator(pkg_name, fn_name, value_db, origins_db, meta_db, budget)
    generator <- create_seeded_args_generator(value_db, seed)
    budget <- remaining(generator)

    df <- fuzz(
        pkg_name = pkg_name,
        fn_name = fn_name,
        generator = generator,
        runner = runner_fun
    )

    expect_equal(nrow(df), budget)
})
