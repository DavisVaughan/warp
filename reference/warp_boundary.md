# Locate period boundaries for a date vector

`warp_boundary()` detects a change in time period along `x`, for
example, rolling from one month to the next. It returns the start and
stop positions for each contiguous period chunk in `x`.

## Usage

``` r
warp_boundary(x, period, ..., every = 1L, origin = NULL)
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

A two column data frame with the columns `start` and `stop`. Both are
double vectors representing boundaries of the date time groups.

## Details

The stop positions are just the
[`warp_change()`](https://davisvaughan.github.io/warp/reference/warp_change.md)
values, and the start positions are computed from these.

## Examples

``` r
x <- as.Date("1970-01-01") + -4:5
x
#>  [1] "1969-12-28" "1969-12-29" "1969-12-30" "1969-12-31" "1970-01-01"
#>  [6] "1970-01-02" "1970-01-03" "1970-01-04" "1970-01-05" "1970-01-06"

# Boundaries by month
warp_boundary(x, "month")
#>   start stop
#> 1     1    4
#> 2     5   10

# Bound by every 5 days, relative to "1970-01-01"
# Creates boundaries of:
# [1969-12-27, 1970-01-01)
# [1970-01-01, 1970-01-06)
# [1970-01-06, 1970-01-11)
warp_boundary(x, "day", every = 5)
#>   start stop
#> 1     1    4
#> 2     5    9
#> 3    10   10

# Bound by every 5 days, relative to the smallest value in our vector
origin <- min(x)
origin
#> [1] "1969-12-28"

# Creates boundaries of:
# [1969-12-28, 1970-01-02)
# [1970-01-02, 1970-01-07)
warp_boundary(x, "day", every = 5, origin = origin)
#>   start stop
#> 1     1    5
#> 2     6   10
```
