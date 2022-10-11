test_that("runner captures warning as error", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            warning("a")
            x + y
        },
        list(2, 40)
    )

    expect_equal(length(x), 3)
    expect_equal(x$error, "a")
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(is.null(x$result), TRUE)
})

test_that("runner survies an error", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            stop(x + y)
        },
        list(2, 40)
    )

    expect_equal(length(x), 3)
    expect_equal(x$error, "42")
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(is.null(x$result), TRUE)
})

test_that("runner survies an sigabort", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            .Call(generatr:::C_sigabtr)
            x + y
        },
        list(2, 40)
    )

    expect_equal(length(x), 3)
    # don't understand why -6, but OK
    expect_equal(x$error, "R session crashed with exit code -6")
    expect_equal(x$exit, -6)
    expect_equal(is.null(x$result), TRUE)

    x <- runner_exec(r, function() 42, list())

    expect_equal(is.na(x$error), TRUE)
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(x$result, 42)
})

test_that("runner survies a timeout", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            Sys.sleep(1)
            x + y
        },
        list(2, 40),
        timeout_ms = 500
    )

    expect_equal(length(x), 3)
    expect_equal(x$error, "Timeout")
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(is.null(x$result), TRUE)
})
