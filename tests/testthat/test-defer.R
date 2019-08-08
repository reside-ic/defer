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
