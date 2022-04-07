test_that("runner captures all", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            warning("a")
            warning("b")
            cat("c")
            message("d")
            x + y
        },
        list(2, 40)
    )

    expect_equal(length(x), 6)
    expect_equal(is.na(x$error), TRUE)
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(x$messages, "d\n")
    expect_equal(x$output, "c")
    expect_equal(x$result, 42)
    expect_equal(x$warnings, c("a", "b"))

    x <- runner_exec(
        r,
        function(x, y) {
            warning("a")
            cat("b")
            message("c")
            x * y
        },
        list(2, 40)
    )

    expect_equal(length(x), 6)
    expect_equal(is.na(x$error), TRUE)
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(x$messages, "c\n")
    expect_equal(x$output, "b")
    expect_equal(x$result, 80)
    expect_equal(x$warnings, "a")
})

test_that("runner survies an error", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            warning("a")
            warning("b")
            cat("c")
            message("d")
            stop(x + y)
        },
        list(2, 40)
    )

    expect_equal(length(x), 6)
    expect_equal(x$error, "42")
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(x$messages, "d\n")
    expect_equal(x$output, "c")
    expect_equal(is.null(x$result), TRUE)
    expect_equal(x$warnings, c("a", "b"))
})

test_that("runner survies an sigabort", {
    r <- runner_start()
    on.exit(runner_stop(r))

    x <- runner_exec(
        r,
        function(x, y) {
            warning("a")
            warning("b")
            cat("c")
            message("d")
            cpp11::cpp_function("int shutdown() { abort(); return 1; }")
            shutdown()
            x + y
        },
        list(2, 40)
    )

    expect_equal(length(x), 6)
    # don't understand why -6, but OK
    expect_equal(x$error, "R session crashed with exit code -6")
    expect_equal(x$exit, -6)
    expect_equal(is.na(x$messages), TRUE)
    expect_equal(is.na(x$output), TRUE)
    expect_equal(is.null(x$result), TRUE)
    expect_equal(is.na(x$warnings), TRUE)

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
            warning("a")
            warning("b")
            cat("c")
            message("d")
            Sys.sleep(1)
            x + y
        },
        list(2, 40),
        timeout_ms = 500
    )

    expect_equal(length(x), 6)
    expect_equal(x$error, "Timeout")
    expect_equal(is.na(x$exit), TRUE)
    expect_equal(is.na(x$messages), TRUE)
    expect_equal(is.na(x$output), TRUE)
    expect_equal(is.null(x$result), TRUE)
    expect_equal(is.na(x$warnings), TRUE)
})