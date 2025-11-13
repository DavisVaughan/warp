# Detect changes in a date time vector

`warp_change()` detects changes at the `period` level.

If `last = TRUE`, it returns locations of the last value before a
change, and the last location in `x` is always included. Additionally,
if `endpoint = TRUE`, the first location in `x` will be included.

If `last = FALSE`, it returns locations of the first value after a
change, and the first location in `x` is always included. Additionally,
if `endpoint = TRUE`, the last location in `x` will be included.

## Usage

``` r
warp_change(
  x,
  period,
  ...,
  every = 1L,
  origin = NULL,
  last = TRUE,
  endpoint = FALSE
)
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

- last:

  `[logical(1)]`

  If `TRUE`, the *last* location *before* a change is returned. The last
  location of the input is always returned.

  If `FALSE`, the *first* location *after* a change is returned. The
  first location of the input is always returned.

- endpoint:

  `[logical(1)]`

  If `TRUE` and `last = TRUE`, will additionally return the first
  location of the input.

  If `TRUE` and `last = FALSE`, will additionally return the last
  location of the input.

  If `FALSE`, does nothing.

## Value

A double vector of locations.

## Examples

``` r
x <- as.Date("2019-01-01") + 0:5
x
#> [1] "2019-01-01" "2019-01-02" "2019-01-03" "2019-01-04" "2019-01-05"
#> [6] "2019-01-06"

# Last location before a change, last location of `x` is always included
warp_change(x, period = "yday", every = 2, last = TRUE)
#> [1] 2 4 6

# Also include first location
warp_change(x, period = "yday", every = 2, last = TRUE, endpoint = TRUE)
#> [1] 1 2 4 6

# First location after a change, first location of `x` is always included
warp_change(x, period = "yday", every = 2, last = FALSE)
#> [1] 1 3 5

# Also include last location
warp_change(x, period = "yday", every = 2, last = FALSE, endpoint = TRUE)
#> [1] 1 3 5 6
```
