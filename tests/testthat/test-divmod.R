test_that("div and divmod are equivalent when arguments are the same sign", {
  expect_equal(divmod(1L, 1L), div(1L, 1L))
  expect_equal(divmod(-1L, -1L), div(-1L, -1L))

  expect_equal(divmod(2L, 5L), div(2L, 5L))
  expect_equal(divmod(-2L, -5L), div(-2L, -5L))

  expect_equal(divmod(5L, 2L), div(5L, 2L))
  expect_equal(divmod(-5L, -2L), div(-5L, -2L))
})

test_that("div and divmod are different when arguments have different signs", {
  expect_equal(divmod(-2L, 5L), c(-1, 3))
  expect_equal(div(-2L, 5L), c(0, -2))

  expect_equal(divmod(-5L, 2L), c(-3, 1))
  expect_equal(div(-5L, 2L), c(-2, -1))
})

test_that("can't divide by 0", {
  expect_error(divmod(1L, 0L), "Division by zero")
})
