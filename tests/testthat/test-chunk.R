# ------------------------------------------------------------------------------
# warp_chunk(<Date>, by = "year")

test_that("can warp_chunk() by year with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_chunk(x, "year"), 0L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_chunk(x, "year"), 1L)
})

test_that("can warp_chunk() by year with 'negative' Dates", {
  x <- as.Date("1969-01-01")
  expect_identical(warp_chunk(x, "year"), -1L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_chunk(x, "year", origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "year", origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "year", origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_chunk(x, "year"), 0L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXct>, by = "year")

test_that("can warp_chunk() by year with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "year"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "year"), 1L)
})

test_that("can warp_chunk() by year with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "year"), -1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "year"), -1L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "year"), -2L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_chunk(x, "year"), NA)

  expect_identical(warp_chunk(x, "year"), 1L)
  expect_identical(warp_chunk(x, "year", x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_chunk(x, "year", origin1), NA)

  expect_identical(warp_chunk(x, "year", origin1), 0L)
  expect_identical(warp_chunk(x, "year", origin2), -1L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "year", origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "year", origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_chunk(x, "year", origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "year"), -1L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXlt>, by = "year")

test_that("can warp_chunk() by year with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "year"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "year"), 1L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<Date>, by = "month")

test_that("can warp_chunk() by month with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- as.Date("1970-02-01")
  expect_identical(warp_chunk(x, "month"), 1L)

  x <- as.Date("1971-02-01")
  expect_identical(warp_chunk(x, "month"), 13L)
})

test_that("can warp_chunk() by month with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_chunk(x, "month"), -1L)

  x <- as.Date("1968-11-30")
  expect_identical(warp_chunk(x, "month"), -14L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_chunk(x, "month", origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "month", origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "month", origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- structure(31L, class = "Date")
  expect_identical(warp_chunk(x, "month"), 1L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXct>, by = "month")

test_that("can warp_chunk() by month with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "month"), 12L)
})

test_that("can warp_chunk() by month with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "month"), -1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "month"), -12L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "month"), -13L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_chunk(x, "month"), NA)

  expect_identical(warp_chunk(x, "month"), 12L)
  expect_identical(warp_chunk(x, "month", x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_chunk(x, "month", origin1), NA)

  expect_identical(warp_chunk(x, "month", origin1), 0L)
  expect_identical(warp_chunk(x, "month", origin2), -12L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "month", origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "month", origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_chunk(x, "month", origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "month"), -1L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXlt>, by = "month")

test_that("can warp_chunk() by year with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "month"), 12L)
})
