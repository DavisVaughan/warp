
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timewarp

<!-- badges: start -->

<!-- badges: end -->

The goal of timewarp is to provide tooling to group date times by
period, and manipulate the groups.

``` r
library(timewarp)
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/timewarp")
```

## Example

One of the core functions in timewarp is `warp_group()`, which allows
you to provide a date time vector and specify a period to group it by.
For example, we can group this vector by month.

``` r
x <- as.Date("1970-01-01") + -2:2
x
#> [1] "1969-12-30" "1969-12-31" "1970-01-01" "1970-01-02" "1970-01-03"

warp_group(x, by = "month")
#> [1] -1 -1  0  0  0
```

The groups that `warp_group()` returns have values that correspond to
the distance from `x` to the `origin`, in units defined by the `by`
period and the width defined by `every`. The `origin` defaults to the
unix epoch of `1970-01-01 00:00:00`, but you can change that. In this
case the groups are saying that, for example, `"1970-01-02"` is in the
same month as the origin, and `"1969-12-31"` is 1 month group away.

You can also group by days. Rather than grouping by 1 day, let’s group
every 2 days together, starting from the default `origin`.

``` r
# Groups 1970-01-01 and 1970-01-02 together
warp_group(x, by = "day", every = 2)
#> [1] -1 -1  0  0  1
```

You will often want to set your own `origin` date. Let’s shift it
forward 1 to `1970-01-02`.

``` r
origin <- as.Date("1970-01-02")
origin
#> [1] "1970-01-02"

# Groups 1970-01-02 and 1970-01-03 together
warp_group(x, by = "day", every = 2, origin = origin)
#> [1] -2 -1 -1  0  0
```

## Inspiration

The algorithm for `warp_group()` was inspired by `xts::endpoints()`.