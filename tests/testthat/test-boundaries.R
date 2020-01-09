test_that("warp_boundary() works", {
  x <- as.Date(c("1969-12-31", "1970-01-02", "1970-01-03", "1970-01-04"))

  expect <- data.frame(start = c(1, 2, 3), stop = c(1, 2, 4))
  expect_equal(warp_boundary(x, "day", every = 2), expect)

  origin <- as.Date("1970-01-02")
  expect <- data.frame(start = c(1, 2, 4), stop = c(1, 3, 4))
  expect_equal(warp_boundary(x, "day", every = 2, origin = origin), expect)
})

test_that("warp_boundary() only allows date like inputs", {
  expect_error(warp_boundary(1, period = "year"), "must inherit from")
})

test_that("the first value is always included as the first `start`", {
  x <- new_date(c(1, 1, 2, 2))
  expect <- data.frame(start = c(1, 3), stop = c(2, 4))
  expect_equal(warp_boundary(x, "day"), expect)
})

test_that("duplicate non-contiguous values are allowed, and show up in different groups", {
  x <- new_date(c(1, 2, 1))
  expect <- data.frame(start = 1:3, stop = 1:3)
  expect_equal(warp_boundary(x, "day"), expect)
})

test_that("size 0 input works", {
  expect <- data.frame(start = numeric(), stop = numeric())
  expect_equal(warp_boundary(new_date(numeric()), period = "year"), expect)
})

test_that("size 1 input works", {
  expect <- data.frame(start = 1, stop = 1)
  expect_equal(warp_boundary(new_date(2), period = "year"), expect)
})
