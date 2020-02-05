
<!-- README.md is generated from README.Rmd. Please edit that file -->

# warp

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/DavisVaughan/warp.svg?branch=master)](https://travis-ci.org/DavisVaughan/warp)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/warp/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/warp?branch=master)
[![R build
status](https://github.com/DavisVaughan/warp/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/warp)
<!-- badges: end -->

![](https://media.giphy.com/media/jjeK2Er3E5igw/giphy.gif)

The goal of warp is to provide tooling to group dates by a variety of
periods, such as: yearly, monthly, by second, by week of the month, and
more.

``` r
library(warp)
```

## Installation

You can install the release version from CRAN with:

``` r
install.package("warp")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/warp")
```

## Example

One of the core functions in warp is `warp_distance()`, which allows you
to provide a date time vector and compute the “distance” from an
`origin`. For example, this computes the number of months from the unix
epoch.

``` r
x <- as.Date("1970-01-01") + -2:2
x
#> [1] "1969-12-30" "1969-12-31" "1970-01-01" "1970-01-02" "1970-01-03"

warp_distance(x, period = "month")
#> [1] -1 -1  0  0  0
```

The values that `warp_distance()` returns correspond to the distance
from `x` to the `origin`, in units defined by the `period` and the width
defined by `every`. The `origin` defaults to the unix epoch of
`1970-01-01 00:00:00` in the time zone of `x`, but you can change that.
In this case the distances are saying that, for example, `"1970-01-02"`
is in the same month as the origin, and `"1969-12-31"` is 1 month group
away.

You can also compute daily distances. Rather than grouping by 1 day,
let’s lump every 2 days together, starting from the default `origin`.

``` r
# Groups 1970-01-01 and 1970-01-02 together
warp_distance(x, period = "day", every = 2)
#> [1] -1 -1  0  0  1
```

You will often want to set your own `origin` date. Let’s shift it
forward 1 to `1970-01-02`.

``` r
origin <- as.Date("1970-01-02")
origin
#> [1] "1970-01-02"

# Groups 1970-01-02 and 1970-01-03 together
warp_distance(x, period = "day", every = 2, origin = origin)
#> [1] -2 -1 -1  0  0
```

Another interesting period to group by is the `"mweek"`, i.e. the week
of the month. Notice that days 1-7 of January 1970 are grouped into the
same bucket. Also note that days 29-31 of December 1969 fell at the end
of their corresponding month. This irregular week of size 3 is treated
as the 5th week of that month, but the offset value of `-1` is still the
number of week buckets from the `origin` of `1970-01-01`.

``` r
y <- as.Date("1969-12-28") + 0:14

tibble::tibble(
  y = y,
  mweek = warp_distance(y, "mweek")
)
#> # A tibble: 15 x 2
#>    y          mweek
#>    <date>     <dbl>
#>  1 1969-12-28    -2
#>  2 1969-12-29    -1
#>  3 1969-12-30    -1
#>  4 1969-12-31    -1
#>  5 1970-01-01     0
#>  6 1970-01-02     0
#>  7 1970-01-03     0
#>  8 1970-01-04     0
#>  9 1970-01-05     0
#> 10 1970-01-06     0
#> 11 1970-01-07     0
#> 12 1970-01-08     1
#> 13 1970-01-09     1
#> 14 1970-01-10     1
#> 15 1970-01-11     1
```

## Inspiration

The algorithm for `warp_distance()` was inspired by `xts::endpoints()`.
