test_that("can locate boundaries", {
  expect <- data.frame(start = 1:2, stop = 1:2)
  expect_equal(locate_boundaries(c(1, 2)), expect)

  expect <- data.frame(start = c(1, 2), stop = c(1, 3))
  expect_equal(locate_boundaries(c(1, 2, 2)), expect)
})

test_that("the first value is always included as the first `start`", {
  expect <- data.frame(start = c(1, 3), stop = c(2, 4))
  expect_equal(locate_boundaries(c(1, 1, 2, 2)), expect)
})

test_that("duplicate non-contiguous values are allowed, and show up in different groups", {
  expect <- data.frame(start = 1:3, stop = 1:3)
  expect_equal(locate_boundaries(c(1, 2, 1)), expect)
})

test_that("can only use an integer or double vector", {
  expect_error(locate_boundaries("x"), "must be an integer or double vector")
})

test_that("size 0 input works", {
  expect <- data.frame(start = numeric(), stop = numeric())
  expect_equal(locate_boundaries(integer()), expect)
  expect_equal(locate_boundaries(numeric()), expect)
})

test_that("size 1 input works", {
  expect <- data.frame(start = 1, stop = 1)
  expect_equal(locate_boundaries(2L), expect)
  expect_equal(locate_boundaries(2), expect)
})
