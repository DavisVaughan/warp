# as.POSIXlt.Date() is unbearably slow, this is much faster
as_posixlt_from_date <- function(x) {
  origin <- structure(0, class = "Date")
  x <- unclass(x)

  # Ignore fractional Date pieces by truncating towards 0
  if (typeof(x) == "double") {
    x <- trunc(x)
  }

  out <- as.POSIXlt(x * 86400, tz = "UTC", origin = origin)

  out
}

test_that("getting the year is identical to as.POSIXlt - integer Date", {
  x <- structure(-1e7:1e7, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year_offset(x), expect)
})

test_that("getting the year is identical to as.POSIXlt - double Date", {
  x <- structure(-1e7:1e7 + 0, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year_offset(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - integer Date", {
  x <- structure(-1e7:1e7, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- (expect$year - 70L) * 12L + expect$mon

  expect_identical(date_get_month_offset(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - double Date", {
  x <- structure(-1e7:1e7 + 0, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- (expect$year - 70L) * 12L + expect$mon

  expect_identical(date_get_month_offset(x), expect)
})

test_that("can get the year offset of the maximum integer value", {
  x <- structure(.Machine$integer.max, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year_offset(x), expect)
})

test_that("can get the year offset of a value close to the minimum integer value", {
  minimum_allowed_date <- -.Machine$integer.max + unclass(as.Date("2001-01-01"))

  x <- structure(minimum_allowed_date, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year_offset(x), expect)
})

test_that("going below the minimum allowed date is an error", {
  minimum_allowed_date_minus_one <- -.Machine$integer.max + unclass(as.Date("2001-01-01")) - 1L

  x <- structure(minimum_allowed_date_minus_one, class = "Date")

  expect_error(date_get_year_offset(x), "Integer overflow")
})
