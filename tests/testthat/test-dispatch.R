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
    str(ret)
    browser()
    1
})
