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
#' @section `origin` Truncation:
#'
#' For `by` values of `"year"` and `"month"`, the information provided in
#' `origin` is truncated. Practically this means that if you specify:
#'
#' ```
#' warp_distance(by = "month", every = 2, origin = as.Date("1970-01-15"))
#' ```
#'
#' then only `1970-01` will be used, and not the fact that the origin starts
#' on the 15th of the month.
#'
#' The `by` value of `"quarter"` is internally `by = "month", every = every * 3`.
#' This means that for `"quarter"` the month specified for the `origin` will
#' be used as the month to start counting from to generate the 3 month quarter.
#'
#' The `by` value of `"week"` is internally `by = "day", every = every * 7`.
#' This means that for `"week"` the day of the month specified for the `origin`
#' will be used to start counting out the 7 day week. It can be useful to
#' set the origin to, say, a Monday to generate week groups that start on
#' Monday.
#'
#' For by values of `"day"` and below with `POSIXct` objects, the internal
#' calculations are done on the number of seconds from the unix origin. For
#' `"day"` this means that you could use an `origin` value in the middle of
#' the day, and daily distances would be computed from that exact time point.
#'
#' @section Precision:
#'
#' With `POSIXct`, the limit of precision is approximately the microsecond
#' level. Only dates that are very close to the unix origin of 1970-01-01 can
#' possibly represent microsecond resolution correctly (close being within
#' about 40 years on either side). Otherwise, the values past the microsecond
#' resolution are essentially random, and can cause problems for the distance
#' calculations. Because of this, decimal digits past the microsecond range are
#' zeroed out, so please do not attempt to rely on them. It should still be safe
#' to work with microseconds, by, say, bucketing them by millisecond distances.
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
#'   - `"millisecond"`
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
