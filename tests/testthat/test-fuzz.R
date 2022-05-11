test_that("fuzz works on +", {
    runner <- runner_start()
    db <- create_db(1, 2)

    on.exit({
        runner_stop(runner)
        unlink(sxpdb::path_db(db), recursive = TRUE)
    })

    runner_fun <- create_fuzz_runner(sxpdb::path_db(db), runner)
    generator <- create_seeded_args_generator(db, list(c(0L, 1L)))
    df <- fuzz(pkg_name = "generatr", fn_name = "wrap_+", generator = generator, runner = runner_fun)

    expect_equal(nrow(df), 1L)
    expect_equal(df$args_idx, "0;1")
    expect_equal(is.na(df$error), TRUE)
    expect_equal(is.na(df$exit), TRUE)
    expect_equal(is.na(df$output), TRUE)
    expect_equal(df$status, 0L)
    expect_equal(df$signature, "(double, double) -> double")
    expect_equal(df$dispatch, list(list(x = character(0), y = character(0))))
})

test_that("fuzz works on + (catches errors)", {
    runner <- runner_start()
    db <- create_db(list(1), list(2))

    on.exit({
        runner_stop(runner)
        unlink(sxpdb::path_db(db), recursive = TRUE)
    })

    runner_fun <- create_fuzz_runner(sxpdb::path_db(db), runner)
    generator <- create_seeded_args_generator(db, list(c(0L, 1L)))
    df <- fuzz(pkg_name = "generatr", fn_name = "wrap_+", generator = generator, runner = runner_fun)

    expect_equal(nrow(df), 1L)
    expect_equal(df$args_idx, "0;1")
    expect_equal(df$error, "Error in x + y : non-numeric argument to binary operator\nCalls: <Anonymous>\n")
    expect_equal(is.na(df$exit), TRUE)
    expect_equal(df$output, "Error in x + y : non-numeric argument to binary operator\nCalls: <Anonymous>")
    expect_equal(df$status, 1L)
    expect_equal(is.na(df$signature), TRUE)
    expect_equal(df$dispatch, list(list(x = character(0), y = character(0))))
})

test_that("fuzz catches warnings", {
    runner <- runner_start()
    db <- create_db(list(1))

    on.exit({
        runner_stop(runner)
        unlink(sxpdb::path_db(db), recursive = TRUE)
    })

    runner_fun <- create_fuzz_runner(sxpdb::path_db(db), runner)
    generator <- create_seeded_args_generator(db, list(c(0L)))
    df <- fuzz(pkg_name = "generatr", fn_name = "f_warn", generator = generator, runner = runner_fun)

    expect_equal(nrow(df), 1L)
    expect_equal(df$args_idx, "0")
    expect_equal(df$error, "Error in (function (x)  : (converted from warning) warn from f\n")
    expect_equal(is.na(df$exit), TRUE)
    expect_equal(df$output, "Error in (function (x)  : (converted from warning) warn from f")
    expect_equal(df$status, 1L)
    expect_equal(is.na(df$signature), TRUE)
    expect_equal(df$dispatch, list(list(x = character(0))))
})

test_that("fuzz one function with timeout", {
    runner <- runner_start()
    db <- create_db(list(1), 2)

    on.exit({
        runner_stop(runner)
        unlink(sxpdb::path_db(db), recursive = TRUE)
    })

    seed <- lapply(1:100, function(x) c(0L, 1L))

    runner_fun <- create_fuzz_runner(sxpdb::path_db(db), runner)
    generator <- create_seeded_args_generator(db, seed)
    budget <- remaining(generator)

    expect_warning(
        df <- fuzz(
            pkg_name = "generatr",
            fn_name = "wrap_+",
            generator = generator,
            runner = runner_fun,
            timeout_s = 2
        ),
        regex = "Timeout \\(2 sec\\) reached after \\d+ calls"
    )

    expect_equal(nrow(df) < budget, TRUE)
})

# test_that("fuzz one function", {
#
#     pkg_name <- "stringr"
#     fn_name <- "str_dup"
#     db_path <- "db/str_dup"
#
#     seed <- list(
#         c(7L, 8L),
#         c(0L, 0L),
#         c(5L, 6L),
#         c(5L, 9L),
#         c(4L, 5L),
#         c(5L, 10L),
#         c(1L, 11L),
#         c(5L, 7L),
#         c(10L, 9L),
#         c(5L, 7L),
#         c(9L, 8L),
#         c(7L, 1L),
#         c(5L, 7L),
#         c(11L, 1L),
#         c(7L, 5L),
#         c(10L, 7L),
#         c(11L, 5L),
#         c(5L, 7L),
#         c(7L, 5L),
#         c(7L, 6L),
#         c(5L, 11L),
#         c(11L, 7L),
#         c(7L, 6L),
#         c(5L, 5L),
#         c(5L, 6L),
#         c(9L, 7L),
#         c(6L, 5L),
#         c(5L, 5L),
#         c(5L, 5L),
#         c(7L, 7L),
#         c(5L, 5L),
#         c(7L, 5L),
#         c(7L, 5L),
#         c(7L, 7L),
#         c(7L, 7L),
#         c(7L, 7L),
#         c(5L, 7L),
#         c(5L, 7L),
#         c(5L, 7L),
#         c(7L, 7L),
#         c(7L, 5L),
#         c(5L, 7L)
#     )
#
#     runner <- runner_start()
#     on.exit(runner_stop(runner))
#     runner_fun <- create_fuzz_runner(db_path, runner)
#
#     value_db <- sxpdb::open_db(db_path)
#     origins_db <- sxpdb::view_origins_db(value_db) %>% as_tibble
#     meta_db <- NULL
#
#     # the following uses the feedback directed (fd) generator
#     # budget <- 42
#     # generator <- create_fd_args_generator(pkg_name, fn_name, value_db, origins_db, meta_db, budget)
#     generator <- create_seeded_args_generator(value_db, seed)
#     budget <- remaining(generator)
#
#     df <- fuzz(
#         pkg_name = pkg_name,
#         fn_name = fn_name,
#         generator = generator,
#         runner = runner_fun
#     )
#
#     expect_equal(nrow(df), budget)
# })
