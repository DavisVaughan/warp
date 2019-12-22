dbl_gauntlet <- function() {
  list(
    x1 = c(1, 2, 3),
    x2 = c(1, 1, 3),
    x3 = c(1, 2, 2),
    x4 = c(NA, 2, 3),
    x5 = c(NA, NA, 3),
    x6 = c(1, 2, NA),
    x7 = c(NaN, 2, 3),
    x8 = c(NaN, NaN, 3),
    x9 = c(1, 2, NaN),
    x10 = c(NA, NaN, 3),
    x11 = c(1, NA, 3),
    x12 = c(3, 2, 1),
    x13 = c(3, 3, 2),
    x14 = c(3, 2, 2),
    x15 = c(3, 2, NA),
    x16 = c(3, NA, NA),
    x17 = c(NA, 2, 1),
    x18 = c(3, 2, NaN),
    x19 = c(3, NaN, NaN),
    x20 = c(NaN, 2, 1),
    x21 = c(3, NaN, NA),
    x22 = c(3, NA, 1)
  )
}

int_gauntlet <- function() {
  list(
    x1 = c(1L, 2L, 3L),
    x2 = c(1L, 1L, 3L),
    x3 = c(1L, 2L, 2L),
    x4 = c(NA_integer_, 2L, 3L),
    x5 = c(NA_integer_, NA_integer_, 3L),
    x6 = c(1L, 2L, NA_integer_),
    x7 = c(1L, NA_integer_, 3L),
    x8 = c(3L, 2L, 1L),
    x9 = c(3L, 3L, 2L),
    x10 = c(3L, 2L, 2L),
    x11 = c(3L, 2L, NA_integer_),
    x12 = c(3L, NA_integer_, NA_integer_),
    x13 = c(NA_integer_, 2L, 1L),
    x14 = c(3L, NA_integer_, 1L)
  )
}

# ------------------------------------------------------------------------------

test_that("dbl / strictly / increasing / largest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = TRUE, na_value = "largest")
  }

  expect_true(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_true(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_true(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / strictly / increasing / smallest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = TRUE, na_value = "smallest")
  }

  expect_true(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_true(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_true(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / strictly / decreasing / largest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = FALSE, na_value = "largest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_true(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_true(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / strictly / decreasing / smallest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = FALSE, na_value = "smallest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_true(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_true(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / nonstrictly / increasing / largest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = TRUE, na_value = "largest")
  }

  expect_true(is_sorted(g$x1))
  expect_true(is_sorted(g$x2))
  expect_true(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_true(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_true(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_true(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_true(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_true(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / nonstrictly / increasing / smallest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = TRUE, na_value = "smallest")
  }

  expect_true(is_sorted(g$x1))
  expect_true(is_sorted(g$x2))
  expect_true(is_sorted(g$x3))
  expect_true(is_sorted(g$x4))
  expect_true(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_true(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_true(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / nonstrictly / decreasing / largest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = FALSE, na_value = "largest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_true(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_true(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_true(is_sorted(g$x13))
  expect_true(is_sorted(g$x14))
  expect_false(is_sorted(g$x15))
  expect_false(is_sorted(g$x16))
  expect_true(is_sorted(g$x17))
  expect_false(is_sorted(g$x18))
  expect_false(is_sorted(g$x19))
  expect_true(is_sorted(g$x20))
  expect_false(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

test_that("dbl / nonstrictly / decreasing / smallest", {
  g <- dbl_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = FALSE, na_value = "smallest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_true(is_sorted(g$x13))
  expect_true(is_sorted(g$x14))
  expect_true(is_sorted(g$x15))
  expect_true(is_sorted(g$x16))
  expect_false(is_sorted(g$x17))
  expect_true(is_sorted(g$x18))
  expect_true(is_sorted(g$x19))
  expect_false(is_sorted(g$x20))
  expect_true(is_sorted(g$x21))
  expect_false(is_sorted(g$x22))
})

# ------------------------------------------------------------------------------

test_that("int / strictly / increasing / largest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = TRUE, na_value = "largest")
  }

  expect_true(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_true(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / strictly / increasing / smallest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = TRUE, na_value = "smallest")
  }

  expect_true(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_true(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / strictly / decreasing / largest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = FALSE, na_value = "largest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_true(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / strictly / decreasing / smallest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = TRUE, increasing = FALSE, na_value = "smallest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_true(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / nonstrictly / increasing / largest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = TRUE, na_value = "largest")
  }

  expect_true(is_sorted(g$x1))
  expect_true(is_sorted(g$x2))
  expect_true(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_true(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / nonstrictly / increasing / smallest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = TRUE, na_value = "smallest")
  }

  expect_true(is_sorted(g$x1))
  expect_true(is_sorted(g$x2))
  expect_true(is_sorted(g$x3))
  expect_true(is_sorted(g$x4))
  expect_true(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_false(is_sorted(g$x8))
  expect_false(is_sorted(g$x9))
  expect_false(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / nonstrictly / decreasing / largest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = FALSE, na_value = "largest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_true(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_true(is_sorted(g$x9))
  expect_true(is_sorted(g$x10))
  expect_false(is_sorted(g$x11))
  expect_false(is_sorted(g$x12))
  expect_true(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

test_that("int / nonstrictly / decreasing / smallest", {
  g <- int_gauntlet()

  is_sorted <- function(x) {
    warp_is_sorted(x, strictly = FALSE, increasing = FALSE, na_value = "smallest")
  }

  expect_false(is_sorted(g$x1))
  expect_false(is_sorted(g$x2))
  expect_false(is_sorted(g$x3))
  expect_false(is_sorted(g$x4))
  expect_false(is_sorted(g$x5))
  expect_false(is_sorted(g$x6))
  expect_false(is_sorted(g$x7))
  expect_true(is_sorted(g$x8))
  expect_true(is_sorted(g$x9))
  expect_true(is_sorted(g$x10))
  expect_true(is_sorted(g$x11))
  expect_true(is_sorted(g$x12))
  expect_false(is_sorted(g$x13))
  expect_false(is_sorted(g$x14))
})

# ------------------------------------------------------------------------------

test_that("input (of any size) must be integer / double", {
  expect_error(warp_is_sorted(c("x", "y")), "integer or a double")
  expect_error(warp_is_sorted(c("x")), "integer or a double")
  expect_error(warp_is_sorted(character()), "integer or a double")
})

test_that("size 0 input is sorted", {
  expect_true(warp_is_sorted(integer()))
  expect_true(warp_is_sorted(numeric()))
})

test_that("size 1 input is sorted", {
  expect_true(warp_is_sorted(1))
  expect_true(warp_is_sorted(1L))
  expect_true(warp_is_sorted(NA_real_))
  expect_true(warp_is_sorted(NaN))
  expect_true(warp_is_sorted(NA_integer_))
})

test_that("`strictly` is validated", {
  expect_error(warp_is_sorted(1, strictly = "nope"), "single logical value")
  expect_error(warp_is_sorted(1, strictly = c(TRUE, TRUE)), "single logical value")
  expect_error(warp_is_sorted(1, strictly = NA), "not be `NA`")
})

test_that("`increasing` is validated", {
  expect_error(warp_is_sorted(1, increasing = "nope"), "single logical value")
  expect_error(warp_is_sorted(1, increasing = c(TRUE, TRUE)), "single logical value")
  expect_error(warp_is_sorted(1, increasing = NA), "not be `NA`")
})

test_that("`na_value` is validated", {
  expect_error(warp_is_sorted(1, na_value = 1), "single character value")
  expect_error(warp_is_sorted(1, na_value = c("largest", "largest")), "single character value")
  expect_error(warp_is_sorted(1, na_value = NA_character_), "either 'largest' or 'smallest'")
  expect_error(warp_is_sorted(1, na_value = "l"), "either 'largest' or 'smallest'")
})
