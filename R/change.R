#' Detect changes in a date time vector
#'
#' `warp_change()` detects changes at the `period` level. It returns the
#' locations of the value just before a change. The location of the last
#' value in `x` is always returned at the end.
#'
#' @inheritParams warp_distance
#'
#' @return
#' A double vector of locations right before a change.
#'
#' @export
#' @examples
#' x <- as.Date("2019-01-01") + 0:5
#'
#' warp_change(x, period = "day", every = 2)
#'
#' warp_change(x, period = "day", every = 2, origin = as.Date("2019-01-01"))
warp_change <- function(x, period, every = 1L, origin = NULL) {
  .Call(warp_warp_change, x, period, every, origin)
}
