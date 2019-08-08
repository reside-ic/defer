check_positive <- function(x) {
  if (x < 0) {
    deferrable_error(paste("got a negative number:", x))
  }
}
