test_that("can locate changes", {
  x <- new_date(c(1, 1, 2))
  expect_equal(warp_change(x, "day"), c(2, 3))
})

test_that("the last value's position is always included", {
  x <- new_date(c(1, 1, 2, 2))
  result <- warp_change(x, "day")

  expect_equal(result[length(result)], length(x))
})

test_that("duplicate non-contiguous values are allowed", {
  x <- new_date(c(1, 1, 2, 1))
  expect_equal(warp_change(x, "day"), c(2, 3, 4))
})

test_that("`NA` / `NaN` / `Inf` dates look equivalent", {
  x <- new_date(c(NA_real_, Inf, NaN))
  expect_equal(warp_change(x, period = "year"), 3)
})

test_that("size 0 input works", {
  expect_equal(warp_change(new_date(), period = "year"), numeric())
})

test_that("size 1 input works", {
  expect_equal(warp_change(new_date(0), period = "year"), 1)
})

test_that("`last = TRUE` returns last location before a change", {
  x <- as.Date("2019-01-01") + 0:5

  expect_identical(
    warp_change(x, period = "yday", every = 2L, last = TRUE),
    c(2, 4, 6)
  )
  expect_identical(
    warp_change(x, period = "yday", every = 2L, last = TRUE, endpoint = TRUE),
    c(1, 2, 4, 6)
  )
})

test_that("`last = FALSE` returns first location after a change", {
  x <- as.Date("2019-01-01") + 0:5

  expect_identical(
    warp_change(x, period = "yday", every = 2L, last = FALSE),
    c(1, 3, 5)
  )
  expect_identical(
    warp_change(x, period = "yday", every = 2L, last = FALSE, endpoint = TRUE),
    c(1, 3, 5, 6)
  )
})

test_that("optional arguments must be specified by name", {
  expect_error(
    warp_change(new_date(0), "year", 1),
    "`...` is not empty in `warp_change[(][)]`."
  )
})
