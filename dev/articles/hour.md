# Hour Distances and Daylight Savings

``` r
library(warp)
```

If using `period = "hour"`, it should work as expected at all times when
using a time zone that doesn’t have daylight savings, like UTC or EST.
If using a time zone with DST, like America/New_York, some additional
explanation is required, especially when `every > 1`.

## Spring Forward Gap

In America/New_York’s time zone, as time was about to reach
`1970-04-26 02:00:00`, daylight savings kicked in and time shifts
forward 1 hour so that the next time is actually `1970-04-26 03:00:00`.

``` r
before_dst <- as.POSIXct("1970-04-26 01:59:59", tz = "America/New_York")
before_dst
#> [1] "1970-04-26 01:59:59 EST"

before_dst + 1
#> [1] "1970-04-26 03:00:00 EDT"
```

[`warp_distance()`](https://davisvaughan.github.io/warp/dev/reference/warp_distance.md)
treats hours 1 and 3 as being side by side, since no hour 2 ever
existed. This means that hours (0, 1) and (3, 4) get grouped together in
the below example.

``` r
x <- as.POSIXct("1970-04-26 00:00:00", tz = "America/New_York") + 3600 * 0:7

data.frame(
  x = x,
  hour = warp_distance(x, "hour", every = 2)
)
#>                     x hour
#> 1 1970-04-26 00:00:00 1380
#> 2 1970-04-26 01:00:00 1380
#> 3 1970-04-26 03:00:00 1381
#> 4 1970-04-26 04:00:00 1381
#> 5 1970-04-26 05:00:00 1382
#> 6 1970-04-26 06:00:00 1382
#> 7 1970-04-26 07:00:00 1383
#> 8 1970-04-26 08:00:00 1383
```

Because `period = "hour"` just computes the running number of 2 hour
periods from the `origin`, this pattern carries forward into the next
day to have a contiguous stream of values. This can be somewhat
confusing, since hours 0 and 1 don’t get grouped together on the 27th.

``` r
y <- as.POSIXct("1970-04-26 22:00:00", tz = "America/New_York") + 3600 * 0:5

data.frame(
  y = y,
  hour = warp_distance(y, "hour", every = 2)
)
#>                     y hour
#> 1 1970-04-26 22:00:00 1390
#> 2 1970-04-26 23:00:00 1391
#> 3 1970-04-27 00:00:00 1391
#> 4 1970-04-27 01:00:00 1392
#> 5 1970-04-27 02:00:00 1392
#> 6 1970-04-27 03:00:00 1393
```

One way that you can sort of get around this is by using lubridate’s
`force_tz()` function to force a UTC time zone with the same clock time
as your original date. I’ve mocked up a poor man’s version of that
function below.

``` r
# Or call `lubridate::force_tz(x, "UTC")`
force_utc <- function(x) {
  x_lt <- as.POSIXlt(x)
  x_lt <- unclass(x_lt)
  
  attributes(x) <- NULL
  
  out <- x + x_lt$gmtoff
  
  as.POSIXct(out, tz = "UTC", origin = "1970-01-01")
}

x_utc <- force_utc(x)
y_utc <- force_utc(y)

x_utc
#> [1] "1970-04-26 00:00:00 UTC" "1970-04-26 01:00:00 UTC"
#> [3] "1970-04-26 03:00:00 UTC" "1970-04-26 04:00:00 UTC"
#> [5] "1970-04-26 05:00:00 UTC" "1970-04-26 06:00:00 UTC"
#> [7] "1970-04-26 07:00:00 UTC" "1970-04-26 08:00:00 UTC"
```

In UTC, hour 2 exists so groups are created as (0, 1), (2, 3), and so
on, even though hour 2 doesn’t actually exist in America/New_York
because of the DST gap. This has the affect of limiting the (2, 3) group
to a maximum size of 1, since only hour 3 is possible in the data.

``` r
data.frame(
  x_utc = x_utc,
  hour = warp_distance(x_utc, "hour", every = 2)
)
#>                 x_utc hour
#> 1 1970-04-26 00:00:00 1380
#> 2 1970-04-26 01:00:00 1380
#> 3 1970-04-26 03:00:00 1381
#> 4 1970-04-26 04:00:00 1382
#> 5 1970-04-26 05:00:00 1382
#> 6 1970-04-26 06:00:00 1383
#> 7 1970-04-26 07:00:00 1383
#> 8 1970-04-26 08:00:00 1384

data.frame(
  y_utc = y_utc,
  hour = warp_distance(y_utc, "hour", every = 2)
)
#>                 y_utc hour
#> 1 1970-04-26 22:00:00 1391
#> 2 1970-04-26 23:00:00 1391
#> 3 1970-04-27 00:00:00 1392
#> 4 1970-04-27 01:00:00 1392
#> 5 1970-04-27 02:00:00 1393
#> 6 1970-04-27 03:00:00 1393
```

## Fall Backwards Overlap

In America/New_York’s time zone, as time was about to reach
`1970-10-25 02:00:00`, daylight savings kicked in and time shifts
backwards 1 hour so that the next time is actually
`1970-10-25 01:00:00`. This means there are 2 full hours with an hour
value of 1 in that day.

``` r
before_fallback <- as.POSIXct("1970-10-25 01:00:00", tz = "America/New_York")
before_fallback
#> [1] "1970-10-25 01:00:00 EDT"

# add 1 hour of seconds
before_fallback + 3600
#> [1] "1970-10-25 01:00:00 EST"
```

Because these are two distinct hours,
[`warp_distance()`](https://davisvaughan.github.io/warp/dev/reference/warp_distance.md)
treats them as such, so in the example below a group of (1 EDT, 1 EST)
gets created. Since daylight savings is currently active, we also have
the situation described above where hour 0 and hour 1 are not grouped
together.

``` r
x <- as.POSIXct("1970-10-25 00:00:00", tz = "America/New_York") + 3600 * 0:7
x
#> [1] "1970-10-25 00:00:00 EDT" "1970-10-25 01:00:00 EDT"
#> [3] "1970-10-25 01:00:00 EST" "1970-10-25 02:00:00 EST"
#> [5] "1970-10-25 03:00:00 EST" "1970-10-25 04:00:00 EST"
#> [7] "1970-10-25 05:00:00 EST" "1970-10-25 06:00:00 EST"

data.frame(
  x = x,
  hour = warp_distance(x, "hour", every = 2)
)
#>                     x hour
#> 1 1970-10-25 00:00:00 3563
#> 2 1970-10-25 01:00:00 3564
#> 3 1970-10-25 01:00:00 3564
#> 4 1970-10-25 02:00:00 3565
#> 5 1970-10-25 03:00:00 3565
#> 6 1970-10-25 04:00:00 3566
#> 7 1970-10-25 05:00:00 3566
#> 8 1970-10-25 06:00:00 3567
```

This fallback adjustment actually realigns hours 0 and 1 in the next
day, since the 25th has 25 hours.

``` r
y <- as.POSIXct("1970-10-25 22:00:00", tz = "America/New_York") + 3600 * 0:5
y
#> [1] "1970-10-25 22:00:00 EST" "1970-10-25 23:00:00 EST"
#> [3] "1970-10-26 00:00:00 EST" "1970-10-26 01:00:00 EST"
#> [5] "1970-10-26 02:00:00 EST" "1970-10-26 03:00:00 EST"

data.frame(
  y = y,
  hour = warp_distance(y, "hour", every = 2)
)
#>                     y hour
#> 1 1970-10-25 22:00:00 3575
#> 2 1970-10-25 23:00:00 3575
#> 3 1970-10-26 00:00:00 3576
#> 4 1970-10-26 01:00:00 3576
#> 5 1970-10-26 02:00:00 3577
#> 6 1970-10-26 03:00:00 3577
```

As before, one way to sort of avoid this is to force a UTC time zone.

``` r
x_utc <- force_utc(x)
x_utc
#> [1] "1970-10-25 00:00:00 UTC" "1970-10-25 01:00:00 UTC"
#> [3] "1970-10-25 01:00:00 UTC" "1970-10-25 02:00:00 UTC"
#> [5] "1970-10-25 03:00:00 UTC" "1970-10-25 04:00:00 UTC"
#> [7] "1970-10-25 05:00:00 UTC" "1970-10-25 06:00:00 UTC"
```

The consequences of this are that you have two dates with an hour value
of 1. When forced to UTC, these look identical. The groups are as you
probably expect with buckets of hours (0, 1), (2, 3), and so on, but now
the two dates with hour values of 1 are identical so they fall in the
same hour group.

``` r
data.frame(
  x_utc = x_utc,
  hour = warp_distance(x_utc, "hour", every = 2)
)
#>                 x_utc hour
#> 1 1970-10-25 00:00:00 3564
#> 2 1970-10-25 01:00:00 3564
#> 3 1970-10-25 01:00:00 3564
#> 4 1970-10-25 02:00:00 3565
#> 5 1970-10-25 03:00:00 3565
#> 6 1970-10-25 04:00:00 3566
#> 7 1970-10-25 05:00:00 3566
#> 8 1970-10-25 06:00:00 3567
```

## Conclusion

While the implementation of `period = "hour"` is *technically* correct,
I recognize that it isn’t the most intuitive operation. More intuitive
would be a period value of `"dhour"`, which would correspond to the
“hour of the day”. This would count the number of hour groups from the
origin, like `"hour"` does, but it would reset the `every`-hour counter
every time you enter a new day. However, this has proved to be
challenging to code up, but I hope to incorporate this eventually.
