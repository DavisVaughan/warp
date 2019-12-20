test_that("getting the year is identical to as.POSIXlt - integer Date", {
  x <- .Date(-1e7:1e7)

  expect <- unclass(as_posixlt(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year(x), expect)
})

test_that("getting the year is identical to as.POSIXlt - double Date", {
  x <- .Date(-1e7:1e7 + 0)

  expect <- unclass(as_posixlt(x))
  expect <- expect$year - 70L

  expect_identical(date_get_year(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - integer Date", {
  x <- .Date(-1e7:1e7)

  expect <- unclass(as_posixlt(x))
  expect <- list(expect$year - 70L, expect$mon)

  expect_identical(date_get_year_month(x), expect)
})

test_that("getting the year month is identical to as.POSIXlt - double Date", {
  x <- .Date(-1e7:1e7 + 0)

  expect <- unclass(as_posixlt(x))
  expect <- list(expect$year - 70L, expect$mon)

  expect_identical(date_get_year_month(x), expect)
})
