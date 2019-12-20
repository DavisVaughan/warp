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

  expect_identical(date_get_year(x), expect)
})

test_that("getting the year is identical to as.POSIXlt - double Date", {
  x <- structure(-1e7:1e7 + 0, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - integer Date", {
  x <- structure(-1e7:1e7, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- list(expect$year - 70L, expect$mon)

  expect_identical(date_get_year_month(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - double Date", {
  x <- structure(-1e7:1e7 + 0, class = "Date")

  expect <- unclass(as_posixlt_from_date(x))
  expect <- list(expect$year - 70L, expect$mon)

  expect_identical(date_get_year_month(x), expect)
})
