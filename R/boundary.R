#' Locate period boundaries for a date vector
#'
#' @description
#' `warp_boundary()` detects a change in time period along `x`, for example,
#' rolling from one month to the next. It returns the start and stop positions
#' for each contiguous period chunk in `x`.
#'
#' @details
#' The stop positions are just the [warp_change()] values, and the start
#' positions are computed from these.
#'
#' @inheritParams warp_distance
#'
#' @return
#' A two column data frame with the columns `start` and `stop`. Both are
#' double vectors representing boundaries of the date time groups.
#'
#' @export
#' @examples
#' x <- as.Date("1970-01-01") + -4:5
#' x
#'
#' # Boundaries by month
#' warp_boundary(x, "month")
#'
#' # Bound by every 5 days, relative to "1970-01-01"
#' # Creates boundaries of:
#' # [1969-12-27, 1970-01-01)
#' # [1970-01-01, 1970-01-06)
#' # [1970-01-06, 1970-01-11)
#' warp_boundary(x, "day", every = 5)
#'
#' # Bound by every 5 days, relative to the smallest value in our vector
#' origin <- min(x)
#' origin
#'
#' # Creates boundaries of:
#' # [1969-12-28, 1970-01-02)
#' # [1970-01-02, 1970-01-07)
#' warp_boundary(x, "day", every = 5, origin = origin)
warp_boundary <- function(x, period, every = 1L, origin = NULL) {
  .Call(warp_warp_boundary, x, period, every, origin)
}
