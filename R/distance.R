#' Compute distances from a date time origin
#'
#' @description
#' `warp_distance()` is a low level engine for computing date time distances.
#'
#' It returns the distance from `x` to the `origin` in units
#' defined by the period specified with `by`. For example, `by = "year"` would
#' return the number of years from the `origin`, which is the Unix epoch of
#' `1970-01-01 00:00:00 UTC` by default.
#'
#' @details
#' The return value of `warp_distance()` is suitable for use as a grouping
#' column in, for example, a `dplyr::mutate()`. This is especially useful for
#' grouping by a multitude of a particular period, such as "every 5 months"
#' starting from your particular `origin` value. If you want "cleaner" grouping
#' values, you might consider running `vctrs::vec_group_id()` on the result from
#' `warp_distance()`.
#'
#' When the time zone of `x` differs from the time zone of `origin`, a warning
#' is issued, and `x` is coerced to the time zone of `origin` without changing
#' the number of seconds of `x` from the epoch. In other words, the time zone
#' of `x` is directly changed to the time zone of `origin` without changing the
#' underlying numeric representation. __It is highly advised to specify your own
#' `origin` value with the same time zone as `x`.__
#'
#' If a `Date` is used for `x`, its time zone is assumed to be `"UTC"`.
#'
#' The information in the provided `origin` is truncated by the `by` period
#' specified. This means that if you specify `by = "month"`,
#' `every = 2`, with an origin of `1970-01-15`, the only pieces of information
#' that are used are the year and the month, and not the fact that the origin
#' starts on the 15th of the month. The exceptions to this are `"week"` and
#' `"quarter"`, see below.
#'
#' For `by = "week"`, a week is defined as a 7 day period starting
#' from the `origin`'s year-month-day value.
#'
#' For `by = "quarter"`, a quarter is defined as a 3 month period starting
#' from the `origin`'s year-month value.
#'
#' @param x `[Date / POSIXct / POSIXlt]`
#'
#'   A date time vector.
#'
#' @param by `[character(1)]`
#'
#'   A string defining the period to group by. Valid inputs are:
#'
#'   - `"year"`
#'   - `"quarter"`
#'   - `"month"`
#'   - `"week"`
#'   - `"day"`
#'   - `"hour"`
#'   - `"minute"`
#'   - `"second"`
#'
#' @param every `[positive integer(1)]`
#'
#'   The number of `by` periods to lump together when constructing the groups.
#'   For example, with the default `origin` and `by = "year"`, `every = 2`
#'   would put the years `1970` and `1971` in the same group.
#'
#' @param origin `[Date(1) / POSIXct(1) / POSIXlt(1)]`
#'
#'   The reference value for the distances to be computed from. This is
#'   particularly important when `every > 1` and you need to define when the
#'   "first" event was to start counting from.
#'
#' @return
#' A double vector containing the distances.
#'
#' @export
#' @examples
#' x <- as.Date("1970-01-01") + -4:4
#' x
#'
#' # Compute monthly distances (really, year + month)
#' warp_distance(x, "month")
#'
#' # Compute distances every 2 days, relative to "1970-01-01"
#' warp_distance(x, "day", every = 2)
#'
#' # Compute distances every 2 days, this time relative to "1970-01-02"
#' warp_distance(x, "day", every = 2, origin = as.Date("1970-01-02"))
#'
#' y <- as.POSIXct("1970-01-01 00:00:01", "UTC") + c(0, 2, 3, 4, 5, 6, 10)
#'
#' # Compute distances every 5 seconds, starting from the unix epoch of
#' # 1970-01-01 00:00:00
#' # So this buckets:
#' # [1970-01-01 00:00:00, 1970-01-01 00:00:05) = 0
#' # [1970-01-01 00:00:05, 1970-01-01 00:00:10) = 1
#' # [1970-01-01 00:00:10, 1970-01-01 00:00:15) = 2
#' warp_distance(y, "second", every = 5)
#'
#' # Compute distances every 5 seconds, starting from the minimum of `x`
#' # 1970-01-01 00:00:01
#' # So this buckets:
#' # [1970-01-01 00:00:01, 1970-01-01 00:00:06) = 0
#' # [1970-01-01 00:00:06, 1970-01-01 00:00:11) = 1
#' # [1970-01-01 00:00:11, 1970-01-01 00:00:16) = 2
#' origin <- as.POSIXct("1970-01-01 00:00:01", "UTC")
#' warp_distance(y, "second", every = 5, origin = origin)
#'
#' # Differing time zones between `x` and `origin` can be particularly
#' # problematic. For this reason, if the time zones are different a warning
#' # is issued and `x` is coerced to the time zone of `origin`. The default
#' # `origin` time zone for the epoch is UTC.
#' x_in_nyc <- as.POSIXct("1970-01-01 01:00:00", "America/New_York")
#'
#' # The default origin is `1970-01-01 00:00:00 UTC`.
#' # We passed in a clock time 1 hour after that, but in a different time zone.
#' # America/New_York is 5 hours behind UTC, so when it is converted to
#' # UTC the value becomes `1970-01-01 06:00:00 UTC`
#' warp_distance(x_in_nyc, "hour")
#'
#' # We can use an origin value in our own time zone to fix this
#' # (it doesn't have to be 1970-01-01)
#' origin <- as.POSIXct("1970-01-01", "America/New_York")
#' warp_distance(x_in_nyc, "hour", origin = origin)
#'
warp_distance <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(warp_warp_distance, x, by, every, origin)
}
