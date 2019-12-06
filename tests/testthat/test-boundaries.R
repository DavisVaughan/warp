# As this is a thin wrapper around `warp_group()` and `locate_boundaries()`,
# we just do a few simple tests here

test_that("warp_boundaries() works", {
  x <- as.Date(c("1969-12-31", "1970-01-02", "1970-01-03", "1970-01-04"))

  expect <- data.frame(start = c(1, 2, 3), stop = c(1, 2, 4))
  expect_equal(warp_boundaries(x, "day", every = 2), expect)

  origin <- as.Date("1970-01-02")
  expect <- data.frame(start = c(1, 2, 4), stop = c(1, 3, 4))
  expect_equal(warp_boundaries(x, "day", every = 2, origin = origin), expect)
})

test_that("warp_boundaries() only allows date like inputs", {
  expect_error(warp_boundaries(1), "must inherit from")
})
