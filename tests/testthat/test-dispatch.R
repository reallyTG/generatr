test_that("catch error", {
    foo <- function(x, y) x + y
    args <- readRDS("data/lang.RDS")
    ret <- trace_dispatch_call(foo, args)

    browser()

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(ret$dispatch, list(x = c("data.frame::as.list", "data.frame::sin")))
})

test_that("coerce is captured", {
    foo <- function(x) {
        grep("A", x)
    }

    ret <- trace_dispatch_call(foo, list(data.frame(x = "A")))

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(ret$dispatch, list(x = c("as.STRSXP", "<default>::as.character")))
})

test_that("dispatch on object with multiple classes", {
    foo <- function(x) {
        sin(x)
    }

    a <- readRDS("data/anova.RDS")
    ret <- trace_dispatch_call(foo, list(a))

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(ret$dispatch, list(x = c("data.frame::as.list", "data.frame::sin")))
})

test_that("internal dispatch", {
    length.A <- function(x) {
        x + 1
    }

    a <- 41
    class(a) <- "A"

    expect_equal(length(a) == 42, TRUE)

    foo <- function(x, y, z) {
        length(y)
        z + x + y
    }

    ret <- trace_dispatch_call(foo, list(0, a, 1))

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(as.integer(ret$result), 42)
    expect_equal(ret$dispatch, list(x = character(0), y = "A::length", z = character(0)))
})

test_that("group dispatch", {
    `+.A` <- function(x, y) {
        42
    }

    a <- 0
    class(a) <- "A"

    expect_equal(a + 1 == 42, TRUE)

    foo <- function(x, y) {
        x + y
    }

    ret <- trace_dispatch_call(foo, list(a, 1))

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(as.integer(ret$result), 42)
    expect_equal(ret$dispatch, list(x = "A::+", y = character(0)))
})

test_that("group dispatch", {
    `+.A` <- function(x, y) {
        42
    }
    `+.B` <- function(x, y) {
        82
    }

    a <- 0
    class(a) <- "A"
    b <- 1
    class(b) <- "B"

    foo <- function(x, y, z) {
        x + y + z
    }

    ret <- trace_dispatch_call(foo, list(a, 1, b))

    expect_equal(length(ret), 3)
    expect_equal(ret$status, 0)
    expect_equal(as.integer(ret$result), 82)
    expect_equal(ret$dispatch, list(x = "A::+", y = character(0), z = "B::+"))
})
