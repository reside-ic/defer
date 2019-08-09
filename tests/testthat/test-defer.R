context("defer")

test_that("deferrable_errors throw immediately", {
  expect_error(deferrable_error("msg"),
               "msg", class = "deferrable_error")
})


test_that("defer_errors returns expression value if no errors", {
  expect_equal(defer_errors({
    x <- 1 + 2
    x * 2
  }), 6)
  ## above expression evaluated in *this* environment, so:
  expect_equal(x, 3)
})


test_that("defer_errors can defer errors", {
  err <- capture_error(defer_errors({
    check_positive(0)
    check_positive(-1)
    check_positive(-2)
  }))

  expect_is(err, "error")
  expect_is(err, "deferred_errors")
  expect_length(err$errors, 2)
  expect_is(err$errors[[1]], "deferrable_error")
  expect_is(err$errors[[2]], "deferrable_error")
  expect_match(err$message, "2 errors occured")
})


test_that("defer_errors throws on undeferrable errors", {
  err <- capture_error(defer_errors({
    check_positive(0)
    check_positive(-1)
    stop("fatal error")
    check_positive(-2)
  }))

  expect_false(inherits(err, "deferred_errors"))
  expect_is(err, "simpleError")
  expect_match(err$message, "fatal error")
})


test_that("invoke restart", {
  value <- withCallingHandlers({
    x <- 1
    deferrable_error("a deferrable error")
    x * 2
  },
  deferrable_error = function(e)
    invokeRestart("continue_deferrable_error"))

  expect_equal(x, 1)
  expect_equal(value, 2)
})


test_that("flush stops continued execution", {
  err <- capture_error(defer_errors({
    check_positive(-1)
    deferred_errors_flush()
    check_positive(-2)
  }))
  expect_is(err, "deferred_errors")
  expect_equal(length(err$errors), 1)
  expect_equal(err$errors[[1]]$message, "got a negative number: -1")
})


test_that("flush before errors is a noop", {
  err <- capture_error(defer_errors({
    check_positive(0)
    deferred_errors_flush()
    check_positive(-1)
    check_positive(-2)
  }))
  expect_is(err, "deferred_errors")
  expect_equal(length(err$errors), 2)
})


test_that("final handling", {
  err <- defer_errors({
    check_positive(0)
    check_positive(-1)
    check_positive(-2)
  }, handler = return)
  expect_is(err, "deferred_errors")
  expect_equal(length(err$errors), 2)
})


test_that("traceback", {
  f <- function(x) {
    g(x)
  }
  g <- function(x) {
    check_positive(x)
  }
  err <- defer_errors({
    f(0)
    f(-1)
    f(-2)
  }, handler = return)

  expect_equal(
    err$errors[[1]]$calls,
    list(quote(f(-1)),
         quote(g(x)),
         quote(check_positive(x)),
         quote(deferrable_error(paste("got a negative number:", x)))))
  expect_equal(
    err$errors[[1]]$call,
    quote(check_positive(x)))
})
