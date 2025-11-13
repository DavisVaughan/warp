# Changelog

## warp (development version)

- warp is now compliant with R’s C API
  ([\#39](https://github.com/DavisVaughan/warp/issues/39)).

- R \>=4.0.0 is now required.

## warp 0.2.1

CRAN release: 2023-11-02

- Fixed a test related to an R-devel bugfix in
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html)
  ([\#36](https://github.com/DavisVaughan/warp/issues/36)).

## warp 0.2.0

CRAN release: 2020-10-21

- All optional arguments must now be specified by name.

- [`warp_change()`](https://davisvaughan.github.io/warp/dev/reference/warp_change.md)
  has two new arguments, `last` and `endpoint`, for controlling exactly
  what type of change points are returned.

## warp 0.1.0

CRAN release: 2020-01-14

- Added a `NEWS.md` file to track changes to the package.
