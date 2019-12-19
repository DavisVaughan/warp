#' Detect changes in a date time vector
#'
#' `warp_change()` detects changes at the period level, using the period
#' supplied in `by`. It returns the locations of the last value before the
#' change. The last value is always returned.
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
#' warp_change(x, by = "day", every = 2)
#'
#' warp_change(x, by = "day", every = 2, origin = as.Date("2019-01-01"))
warp_change <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(warp_warp_change, x, by, every, origin)
}
