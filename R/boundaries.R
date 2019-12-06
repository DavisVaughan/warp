#' Locate period boundaries for a date vector
#'
#' `warp_boundaries()` detects a change in time period along `x`, for example,
#' rolling from one month to the next. It returns the start and stop positions
#' for each contiguous period chunk in `x`.
#'
#' @inheritParams warp_group
#'
#' @param x `[Date / POSIXct / POSIXlt]`
#'
#'   The vector to compute time boundaries for.
#'
#' @export
#' @examples
#' x <- as.Date("1970-01-01") + -4:5
#' x
#'
#' # Boundaries by month
#' warp_boundaries(x, "month")
#'
#' # Bound by every 5 days, relative to "1970-01-01"
#' # Creates boundaries of:
#' # [1969-12-27, 1970-01-01)
#' # [1970-01-01, 1970-01-06)
#' # [1970-01-06, 1970-01-11)
#' warp_boundaries(x, "day", every = 5)
#'
#' # Bound by every 5 days, relative to the smallest value in our vector
#' origin <- min(x)
#' origin
#'
#' # Creates boundaries of:
#' # [1969-12-28, 1970-01-02)
#' # [1970-01-02, 1970-01-07)
#' warp_boundaries(x, "day", every = 5, origin = origin)
warp_boundaries <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timewarp_warp_boundaries, x, by, every, origin)
}
