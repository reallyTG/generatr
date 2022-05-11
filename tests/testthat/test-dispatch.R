test_that("simple dispatch", {

    length.A <- function(x) {
        x+1
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
    expect_equal(ret$dispatch, list(x=character(0), y="A::length", z=character(0)))
})
