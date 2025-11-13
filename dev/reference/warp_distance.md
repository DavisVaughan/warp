# Compute distances from a date time origin

`warp_distance()` is a low level engine for computing date time
distances.

It returns the distance from `x` to the `origin` in units defined by the
`period`.

For example, `period = "year"` would return the number of years from the
`origin`. Setting `every = 2` would return the number of 2 year groups
from the `origin`.

## Usage

``` r
warp_distance(x, period, ..., every = 1L, origin = NULL)
```

## Arguments

- x:

  `[Date / POSIXct / POSIXlt]`

  A date time vector.

- period:

  `[character(1)]`

  A string defining the period to group by. Valid inputs can be roughly
  broken into:

  - `"year"`, `"quarter"`, `"month"`, `"week"`, `"day"`

  - `"hour"`, `"minute"`, `"second"`, `"millisecond"`

  - `"yweek"`, `"mweek"`

  - `"yday"`, `"mday"`

- ...:

  `[dots]`

  These dots are for future extensions and must be empty.

- every:

  `[positive integer(1)]`

  The number of periods to group together.

  For example, if the period was set to `"year"` with an every value of
  `2`, then the years 1970 and 1971 would be placed in the same group.

- origin:

  `[Date(1) / POSIXct(1) / POSIXlt(1) / NULL]`

  The reference date time value. The default when left as `NULL` is the
  epoch time of `1970-01-01 00:00:00`, *in the time zone of the index*.

  This is generally used to define the anchor time to count from, which
  is relevant when the every value is `> 1`.

## Value

A double vector containing the distances.

## Details

The return value of `warp_distance()` has a variety of uses. It can be
used for:

- A grouping column in a `dplyr::group_by()`. This is especially useful
  for grouping by a multitude of a particular period, such as "every 5
  months".

- Computing distances between values in `x`, in units of the `period`.
  By returning the distances from the `origin`, `warp_distance()` has
  also implicitly computed the distances between values of `x`. This is
  used by `slide::block()` to break the input into time blocks.

When the time zone of `x` differs from the time zone of `origin`, a
warning is issued, and `x` is coerced to the time zone of `origin`
without changing the number of seconds of `x` from the epoch. In other
words, the time zone of `x` is directly changed to the time zone of
`origin` without changing the underlying numeric representation. **It is
highly advised to specify an `origin` value with the same time zone as
`x`.** If a `Date` is used for `x`, its time zone is assumed to be
`"UTC"`.

## Period

For `period` values of `"year"`, `"month"`, and `"day"`, the information
provided in `origin` is truncated. Practically this means that if you
specify:

    warp_distance(period = "month", origin = as.Date("1970-01-15"))

then only `1970-01` will be used, and not the fact that the origin
starts on the 15th of the month.

The `period` value of `"quarter"` is internally
`period = "month", every = every * 3`. This means that for `"quarter"`
the month specified for the `origin` will be used as the month to start
counting from to generate the 3 month quarter.

To mimic the behavior of `lubridate::floor_date()`, use
`period = "week"`. Internally this is just
`period = "day", every = every * 7`. To mimic the `week_start` argument
of `floor_date()`, set `origin` to a date with a week day identical to
the one you want the week to start from. For example, the default origin
of `1970-01-01` is a Thursday, so this would be generate groups
identical to `floor_date(week_start = 4)`.

The `period` value of `"yday"` is computed as complete `every`-day
periods from the `origin`, with a forced reset of the `every`-day
counter every time you hit the month-day value of the `origin`.
`"yweek"` is built on top of this internally as
`period = "yday", every = every * 7`. This ends up using an algorithm
very similar to `lubridate::week()`, with the added benefit of being
able to control the `origin` date.

The `period` value of `"mday"` is computed as `every`-day periods within
each month, with a forced reset of the `every`-day counter on the first
day of each month. The most useful application of this is `"mweek"`,
which is implemented as `period = "mday", every = every * 7`. This
allows you to group by the "week of the month". For `"mday"` and
`"mweek"`, only the year and month parts of the `origin` value are used.
Because of this, the `origin` argument is not that interesting for these
periods.

The `"hour"` period (and more granular frequencies) can produce results
that might be surprising, even if they are technically correct. See the
vignette at
[`vignette("hour", package = "warp")`](https://davisvaughan.github.io/warp/dev/articles/hour.md)
for more information.

## Precision

With `POSIXct`, the limit of precision is approximately the microsecond
level. Only dates that are very close to the unix origin of 1970-01-01
can possibly represent microsecond resolution correctly (close being
within about 40 years on either side). Otherwise, the values past the
microsecond resolution are essentially random, and can cause problems
for the distance calculations. Because of this, decimal digits past the
microsecond range are zeroed out, so please do not attempt to rely on
them. It should still be safe to work with microseconds, by, say,
bucketing them by millisecond distances.

## Examples

``` r
x <- as.Date("1970-01-01") + -4:4
x
#> [1] "1969-12-28" "1969-12-29" "1969-12-30" "1969-12-31" "1970-01-01"
#> [6] "1970-01-02" "1970-01-03" "1970-01-04" "1970-01-05"

# Compute monthly distances (really, year + month)
warp_distance(x, "month")
#> [1] -1 -1 -1 -1  0  0  0  0  0

# Compute distances every 2 days, relative to "1970-01-01"
warp_distance(x, "day", every = 2)
#> [1] -2 -2 -1 -1  0  0  1  1  2

# Compute distances every 2 days, this time relative to "1970-01-02"
warp_distance(x, "day", every = 2, origin = as.Date("1970-01-02"))
#> [1] -3 -2 -2 -1 -1  0  0  1  1

y <- as.POSIXct("1970-01-01 00:00:01", "UTC") + c(0, 2, 3, 4, 5, 6, 10)

# Compute distances every 5 seconds, starting from the unix epoch of
# 1970-01-01 00:00:00
# So this buckets:
# [1970-01-01 00:00:00, 1970-01-01 00:00:05) = 0
# [1970-01-01 00:00:05, 1970-01-01 00:00:10) = 1
# [1970-01-01 00:00:10, 1970-01-01 00:00:15) = 2
warp_distance(y, "second", every = 5)
#> [1] 0 0 0 1 1 1 2

# Compute distances every 5 seconds, starting from the minimum of `x`
# 1970-01-01 00:00:01
# So this buckets:
# [1970-01-01 00:00:01, 1970-01-01 00:00:06) = 0
# [1970-01-01 00:00:06, 1970-01-01 00:00:11) = 1
# [1970-01-01 00:00:11, 1970-01-01 00:00:16) = 2
origin <- as.POSIXct("1970-01-01 00:00:01", "UTC")
warp_distance(y, "second", every = 5, origin = origin)
#> [1] 0 0 0 0 1 1 2

# ---------------------------------------------------------------------------
# Time zones

# When `x` is not UTC and `origin` is left as `NULL`, the origin is set as
# 1970-01-01 00:00:00 in the time zone of `x`. This seems to be the most
# practically useful default.
z <- as.POSIXct("1969-12-31 23:00:00", "UTC")
z_in_nyc <- as.POSIXct("1969-12-31 23:00:00", "America/New_York")

# Practically this means that these give the same result, because their
# `origin` values are defined in their respective time zones.
warp_distance(z, "year")
#> [1] -1
warp_distance(z_in_nyc, "year")
#> [1] -1

# Compare that to what would happen if we used a static `origin` of
# 1970-01-01 00:00:00 UTC.
# America/New_York is 5 hours behind UTC, so when `z_in_nyc` is converted to
# UTC the value becomes `1970-01-01 04:00:00 UTC`, a different year. Because
# this is generally surprising, a warning is thrown.
origin <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
warp_distance(z, "year", origin = origin)
#> [1] -1
warp_distance(z_in_nyc, "year", origin = origin)
#> Warning: `x` (America/New_York) and `origin` (UTC) do not have the same time zone. Converting `x` to the time zone of `origin`. It is highly advised to provide `x` and `origin` with the same time zone.
#> [1] 0

# ---------------------------------------------------------------------------
# `period = "yweek"`

x <- as.Date("2019-12-23") + 0:16
origin <- as.Date("1970-01-01")

# `"week"` counts the number of 7 day periods from the `origin`
# `"yweek"` restarts the 7 day counter every time you hit the month-day
# value of the `origin`. Notice how, for the `yweek` column, only 1 day was
# in the week starting with `2019-12-31`. This is because the next day is
# `2020-01-01`, which aligns with the month-day value of the `origin`.
data.frame(
  x = x,
  week = warp_distance(x, "week", origin = origin),
  yweek = warp_distance(x, "yweek", origin = origin)
)
#>             x week yweek
#> 1  2019-12-23 2607  2647
#> 2  2019-12-24 2607  2648
#> 3  2019-12-25 2607  2648
#> 4  2019-12-26 2608  2648
#> 5  2019-12-27 2608  2648
#> 6  2019-12-28 2608  2648
#> 7  2019-12-29 2608  2648
#> 8  2019-12-30 2608  2648
#> 9  2019-12-31 2608  2649
#> 10 2020-01-01 2608  2650
#> 11 2020-01-02 2609  2650
#> 12 2020-01-03 2609  2650
#> 13 2020-01-04 2609  2650
#> 14 2020-01-05 2609  2650
#> 15 2020-01-06 2609  2650
#> 16 2020-01-07 2609  2650
#> 17 2020-01-08 2609  2651

# ---------------------------------------------------------------------------
# `period = "mweek"`

x <- as.Date("2019-12-23") + 0:16

# `"mweek"` breaks `x` up into weeks of the month. Notice how days 1-7
# of 2020-01 all have the same distance value. A forced reset of the 7 day
# counter is done at the 1st of every month. This results in the 3 day
# week of the month at the end of 2019-12, from 29-31.
data.frame(
  x = x,
  mweek = warp_distance(x, "mweek")
)
#>             x mweek
#> 1  2019-12-23  2960
#> 2  2019-12-24  2960
#> 3  2019-12-25  2960
#> 4  2019-12-26  2960
#> 5  2019-12-27  2960
#> 6  2019-12-28  2960
#> 7  2019-12-29  2961
#> 8  2019-12-30  2961
#> 9  2019-12-31  2961
#> 10 2020-01-01  2962
#> 11 2020-01-02  2962
#> 12 2020-01-03  2962
#> 13 2020-01-04  2962
#> 14 2020-01-05  2962
#> 15 2020-01-06  2962
#> 16 2020-01-07  2962
#> 17 2020-01-08  2963
```
