##' Create an error that will stop immediately, or can be continued from.
##' @title Create a deferrable error
##' @param message The error message
##' @export
##' @examples
##' tryCatch(
##'   deferrable_error("my error"),
##'   error = identity)
##'
##'
##' # Create a deferrable error and continue from it:
##' value <- withCallingHandlers({
##'    x <- 1
##'    defer::deferrable_error("a deferrable error")
##'    x * 2
##'  },
##'  deferrable_error = function(e)
##'    invokeRestart("continue_deferrable_error"))
deferrable_error <- function(message) {
  withRestarts(
    stop(error(message, "deferrable_error")),
    continue_deferrable_error = function(...) NULL)
}


##' Run a block of code, collecting any \code{\link{deferrable_error}}
##' calls that occur.  Ordinary errors will be thrown immediately.
##'
##' @title Run a block of code, collecting deferrable errors
##' @param expr An expression to evaluate
##' @export
##' @examples
##' check_positive <- function(x) {
##'   if (x < 0) {
##'     deferrable_error(paste("got a negative number:", x))
##'   }
##' }
##' err <- tryCatch(
##'   defer::defer_errors({
##'     check_positive(0)
##'     check_positive(-1)
##'     check_positive(-2)
##'   }),
##'   error = identity)
##' err
defer_errors <- function(expr) {
  errors <- list()

  value <- withCallingHandlers(
    expr,
    deferrable_error = function(e) {
      errors <<- c(errors, list(e))
      invokeRestart("continue_deferrable_error")
    },
    deferred_errors_flush = function(e) {
      deferred_errors(errors)
    },
    clear_errors = function(e) {
      errors <<- list()
    })

  deferred_errors(errors, value)

  value
}


##' Within a \code{\link{defer_errors}} block, flush any deferred
##' errors, turning them into realised errors.  If no deferrable
##' errors have occured, this function has no effect.
##' @title Flush deferred errors
##' @export
##' @examples
##' check_positive <- function(x) {
##'   if (x < 0) {
##'     deferrable_error(paste("got a negative number:", x))
##'   }
##' }
##' err <- tryCatch(
##'   defer::defer_errors({
##'     check_positive(-1)
##'     defer::deferred_errors_flush()
##'     check_positive(-2)
##'   }),
##'   error = identity)
##' err
deferred_errors_flush <- function() {
  condition("deferred_errors_flush")
}


deferred_errors <- function(errors, value = NULL) {
  if (length(errors) > 0L) {
    errs <- vapply(errors, "[[", character(1), "message")
    msg <- sprintf("%d %s occured:\n%s",
                   length(errors),
                   ngettext(length(errors), "error", "errors"),
                   paste0("  - ", errs, collapse = "\n"))
    err <- list(message = msg, errors = errors, value = value)
    class(err) <- c("deferred_errors", "error", "condition")
    stop(err)
  }
}


error <- function(message, class, ...) {
  ret <- list(message = message, ...)
  class(ret) <- c(class, "error", "condition")
  ret
}


condition <- function(class) {
  signalCondition(structure(list(), class = c(class, "condition")))
}
