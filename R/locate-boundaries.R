#' Locate boundaries around changes in a vector
#'
#' `locate_boundaries()` returns the running start and stop positions
#' identifying sections of unchanged values in `x`.
#'
#' @param x `[integer / double]`
#'
#'   The vector to locate boundaries in. Can only be an integer or double
#'   vector.
#'
#' @return
#' A two column data frame with double columns `start` and `stop`.
#'
#' @details
#' `1` is always the first `start` position, and `length(x)` is always the last
#' `stop` position.
#'
#' @export
#' @examples
#' locate_boundaries(c(2, 2, 2, 3, 5, 5))
locate_boundaries <- function(x) {
  .Call(timewarp_locate_boundaries, x)
}
