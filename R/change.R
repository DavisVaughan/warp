#' Detect changes in a date time vector
#'
#' @description
#' `warp_change()` detects changes at the `period` level.
#'
#' If `last = TRUE`, it returns locations of the last value before a change,
#' and the last location in `x` is always included. Additionally, if
#' `endpoint = TRUE`, the first location in `x` will be included.
#'
#' If `last = FALSE`, it returns locations of the first value after a change,
#' and the first location in `x` is always included. Additionally, if
#' `endpoint = TRUE`, the last location in `x` will be included.
#'
#' @inheritParams warp_distance
#'
#' @param last `[logical(1)]`
#'
#'   If `TRUE`, the _last_ location _before_ a change is returned.
#'   The last location of the input is always returned.
#'
#'   If `FALSE`, the _first_ location _after_ a change is returned.
#'   The first location of the input is always returned.
#'
#' @param endpoint `[logical(1)]`
#'
#'   If `TRUE` and `last = TRUE`, will additionally return the first location
#'   of the input.
#'
#'   If `TRUE` and `last = FALSE`, will additionally return the last location
#'   of the input.
#'
#'   If `FALSE`, does nothing.
#'
#' @return
#' A double vector of locations.
#'
#' @export
#' @examples
#' x <- as.Date("2019-01-01") + 0:5
#' x
#'
#' # Last location before a change, last location of `x` is always included
#' warp_change(x, period = "yday", every = 2, last = TRUE)
#'
#' # Also include first location
#' warp_change(x, period = "yday", every = 2, last = TRUE, endpoint = TRUE)
#'
#' # First location after a change, first location of `x` is always included
#' warp_change(x, period = "yday", every = 2, last = FALSE)
#'
#' # Also include last location
#' warp_change(x, period = "yday", every = 2, last = FALSE, endpoint = TRUE)
warp_change <- function(x,
                        period,
                        ...,
                        every = 1L,
                        origin = NULL,
                        last = TRUE,
                        endpoint = FALSE) {
  check_dots_empty("warp_change", ...)
  .Call(warp_warp_change, x, period, every, origin, last, endpoint)
}
