test_that("can locate changes in an integer vector", {
  expect_equal(locate_changes(c(1L, 1L, 2L)), c(2, 3))
})

test_that("can locate changes in a double vector", {
  expect_equal(locate_changes(c(1, 1, 2)), c(2, 3))
})

test_that("the last value's position is always included", {
  x <- c(1, 1, 2, 2)
  result <- locate_changes(c(1, 1, 2, 2))

  expect_equal(result[length(result)], length(x))
})

test_that("duplicate non-contiguous values are allowed", {
  expect_equal(locate_changes(c(1, 1, 2, 1)), c(2, 3, 4))
})

test_that("`NA_integer_` values look equivalent", {
  expect_equal(locate_changes(c(1, NA_integer_, NA_integer_)), c(1, 3))
})

test_that("`NA_real_` values look equivalent", {
  expect_equal(locate_changes(c(1, NA_real_, NA_real_)), c(1, 3))
})

test_that("`NaN` values look equivalent", {
  expect_equal(locate_changes(c(1, NaN, NaN)), c(1, 3))
})

test_that("`NaN` and `NA_real_` values look different", {
  expect_equal(locate_changes(c(1, NaN, NA_real_)), c(1, 2, 3))
})

test_that("can only use double / integer", {
  expect_error(locate_changes("x"), "must be an integer or double vector")
})
