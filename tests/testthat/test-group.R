# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "year")

test_that("can warp_group() by year with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "year"), 0L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_group(x, "year"), 1L)
})

test_that("can warp_group() by year with 'negative' Dates", {
  x <- as.Date("1969-01-01")
  expect_identical(warp_group(x, "year"), -1L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "year", origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "year", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "year"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_group(x, "year"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_group(x, "year"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  expect_equal(warp_group(x, every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  origin <- as.Date("1971-01-01")

  expect_equal(warp_group(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  y <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_group(y), 0L)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "year")

test_that("can warp_group() by year with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "year"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "year"), 1L)
})

test_that("can warp_group() by year with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "year"), -1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "year"), -1L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "year"), -2L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "year"), NA)

  expect_identical(warp_group(x, "year"), 1L)
  expect_identical(warp_group(x, "year", x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "year", origin1), NA)

  expect_identical(warp_group(x, "year", origin = origin1), 0L)
  expect_identical(warp_group(x, "year", origin = origin2), -1L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "year", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "year", origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "year"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "year"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "year"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  expect_equal(warp_group(x, every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  origin <- as.Date("1971-01-01")

  expect_equal(warp_group(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_equal(warp_group(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "year")

test_that("can warp_group() by year with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "year"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "year"), 1L)
})

# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "month")

test_that("can warp_group() by month with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "month"), 0L)

  x <- as.Date("1970-02-01")
  expect_identical(warp_group(x, "month"), 1L)

  x <- as.Date("1971-02-01")
  expect_identical(warp_group(x, "month"), 13L)
})

test_that("can warp_group() by month with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_group(x, "month"), -1L)

  x <- as.Date("1968-11-30")
  expect_identical(warp_group(x, "month"), -14L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "month", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "month", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "month"), 0L)

  x <- structure(31L, class = "Date")
  expect_identical(warp_group(x, "month"), 1L)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  expect_equal(warp_group(x, by = "month", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "month", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "month", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  origin <- as.Date("1970-02-01")

  expect_equal(warp_group(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_group(x, by = "month"), 0L)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "month")

test_that("can warp_group() by month with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "month"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "month"), 12L)
})

test_that("can warp_group() by month with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "month"), -1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "month"), -12L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "month"), -13L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "month"), NA)

  expect_identical(warp_group(x, "month"), 12L)
  expect_identical(warp_group(x, "month", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "month", origin = origin1), NA)

  expect_identical(warp_group(x, "month", origin = origin1), 0L)
  expect_identical(warp_group(x, "month", origin = origin2), -12L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "month", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "month", origin = origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "month"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "month"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "month"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  expect_equal(warp_group(x, by = "month", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "month", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "month", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  origin <- as.Date("1970-02-01")

  expect_equal(warp_group(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1970-02-01", tz = "America/New_York")

  expect_equal(warp_group(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "month")

test_that("can warp_group() by month with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "month"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "month"), 12L)
})

# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "day")

test_that("can warp_group() by day with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "day"), 0L)

  x <- as.Date("1970-01-02")
  expect_identical(warp_group(x, "day"), 1L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_group(x, "day"), 365L)
})

test_that("can warp_group() by day with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_group(x, "day"), -1L)

  x <- as.Date("1969-12-30")
  expect_identical(warp_group(x, "day"), -2L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "day", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "day", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "day"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_group(x, "day"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_group(x, "day"), NA_integer_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_group(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_group(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_group(x, by = "day"), 0L)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "day")

test_that("can warp_group() by day with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "day"), 0L)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_group(x, "day"), 1L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "day"), 365L)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2 days from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1 days from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 days from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1 days from epoch
test_that("can warp_group() by day with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day"), -2L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day"), -2L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day"), -1L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day"), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day"), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day"), 0L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day"), 1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day"), -365L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day"), -366L)
})

test_that("can warp_group() by day with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), 1L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "day", origin = origin), 1L)
})

test_that("can warp_group() by day with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), -2L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), -2L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "day", origin = origin), 1L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "day"), NA)

  expect_identical(warp_group(x, "day"), 365L)
  expect_identical(warp_group(x, "day", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "day", origin = origin1), NA)

  expect_identical(warp_group(x, "day", origin = origin1), 0L)
  expect_identical(warp_group(x, "day", origin = origin2), -365L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "day", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "day", origin = origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "day"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "day"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "day"), NA_integer_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_group(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  expect_equal(warp_group(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle fractional seconds before the epoch correctly", {
  # Base R printing is wrong, because as.POSIXlt() is wrong
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17667

  # I don't care what base R prints this as, this is:
  # "1969-12-31T23:59:59.5"
  x <- .POSIXct(-0.5, "UTC")

  # This is
  # "1969-12-30T23:59:59.5"
  y <- .POSIXct(-86400.5, "UTC")

  expect_equal(warp_group(x, "day"), -1)
  expect_equal(warp_group(y, "day"), -2)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "day")

test_that("can warp_group() by day with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "day"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "day"), 365L)
})

# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "hour")

test_that("can warp_group() by hour with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "hour"), 0L)

  x <- as.Date("1970-01-02")
  expect_identical(warp_group(x, "hour"), 24L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_group(x, "hour"), as.integer(24L * 365L))
})

test_that("can warp_group() by hour with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_group(x, "hour"), -24L)

  x <- as.Date("1969-12-30")
  expect_identical(warp_group(x, "hour"), -48L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "hour", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "hour", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "hour", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "hour"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_group(x, "hour"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_group(x, "hour"), NA_integer_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_group(x, by = "hour", every = 48L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 72L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 96L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "hour", every = 48L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 72L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "hour", every = 96L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_group(x, by = "hour", every = 48L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 72L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 96L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "hour", every = 48L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 72L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "hour", every = 96L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 02:24:00 UTC"
  # structure(.1 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.1, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_group(x, by = "hour"), 0L)
  expect_identical(warp_group(y, by = "hour"), 0L)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "hour")

test_that("can warp_group() by hour with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "hour"), 0L)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_group(x, "hour"), 24L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "hour"), as.integer(24L * 365L))
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -48 hours from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -24 hours from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 hours from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 24 hours from epoch
test_that("can warp_group() by hour with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -48L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -25L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -24L)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -23L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour"), 23L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), 24L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -8760L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour"), -8761L)
})

test_that("can warp_group() by hour with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), -24L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), 0L)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), 1L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), 23L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), 24L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "hour", origin = origin), 47L)
})

test_that("can warp_group() by hour with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), -48L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), -25L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), -24L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), 23L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "hour", origin = origin), 24L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "hour"), NA)

  expect_identical(warp_group(x, "hour"), 8760L)
  expect_identical(warp_group(x, "hour", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "hour", origin = origin1), NA)

  expect_identical(warp_group(x, "hour", origin = origin1), 0L)
  expect_identical(warp_group(x, "hour", origin = origin2), -8760L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "hour", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "hour", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "hour", origin = origin)),
      4L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "hour"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "hour"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "hour"), NA_integer_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_group(x, by = "hour", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  origin <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")

  expect_equal(warp_group(x, by = "hour", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "hour", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  expect_equal(warp_group(x, by = "hour", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")

  expect_equal(warp_group(x, by = "hour", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "hour", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "hour", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle fractional seconds before the epoch correctly", {
  # Base R printing is wrong, because as.POSIXlt() is wrong
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17667

  # I don't care what base R prints this as, this is:
  # "1969-12-31T23:59:59.5"
  x <- .POSIXct(-0.5, "UTC")

  # This is
  # "1969-12-31T22:59:59.5"
  y <- .POSIXct(-3600.5, "UTC")

  expect_equal(warp_group(x, "hour"), -1)
  expect_equal(warp_group(y, "hour"), -2)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "hour")

test_that("can warp_group() by hour with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "hour"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "hour"), 8760L)
})

# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "minute")

test_that("can warp_group() by minute with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "minute"), 0L)

  x <- as.Date("1970-01-02")
  expect_identical(warp_group(x, "minute"), 1440L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_group(x, "minute"), 60L * 24L * 365L)
})

test_that("can warp_group() by minute with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_group(x, "minute"), -1440L)

  x <- as.Date("1969-12-30")
  expect_identical(warp_group(x, "minute"), -1440L * 2L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "minute", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 minutes before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "minute", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "minute", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "minute"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_group(x, "minute"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_group(x, "minute"), NA_integer_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_group(x, by = "minute", every = 2880L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4320L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 5760L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "minute", every = 2880L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4320L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "minute", every = 5760L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_group(x, by = "minute", every = 2880L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4320L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 5760L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "minute", every = 2880L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4320L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "minute", every = 5760L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_group(x, by = "minute"), 0L)
  expect_identical(warp_group(y, by = "minute"), 0L)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "minute")

test_that("can warp_group() by minute with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 0L)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 1440L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 60L * 24L * 365L)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2880 minutes from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1440 minutes from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 minutes from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1440 minutes from epoch
test_that("can warp_group() by minute with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute"), -2880L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute"), -1441L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute"), -1440L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute"), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 1439L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute"), 1440L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute"), -(60L * 24L * 365L))
})

test_that("can warp_group() by minute with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), -1440L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), 0L)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), 60L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), 1439L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), 1440L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "minute", origin = origin), 2880L - 1L)
})

test_that("can warp_group() by minute with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), -2880L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), -1441L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), -1440L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), 1439L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "minute", origin = origin), 1440L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "minute"), NA)

  expect_identical(warp_group(x, "minute"), 525600L)
  expect_identical(warp_group(x, "minute", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "minute", origin = origin1), NA)

  expect_identical(warp_group(x, "minute", origin = origin1), 0L)
  expect_identical(warp_group(x, "minute", origin = origin2), -525600L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours behind UTC, or 300 minutes
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "minute", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "minute", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "minute", origin = origin)),
      240L # 4 hr * 60 min
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "minute"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "minute"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "minute"), NA_integer_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_group(x, by = "minute", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  origin <- as.POSIXct("1970-01-01 00:01:00", tz = "UTC")

  expect_equal(warp_group(x, by = "minute", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "minute", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  expect_equal(warp_group(x, by = "minute", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:01:00", tz = "UTC")

  expect_equal(warp_group(x, by = "minute", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_group(x, by = "minute", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_group(x, by = "minute", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle fractional seconds before the epoch correctly", {
  # Base R printing is wrong, because as.POSIXlt() is wrong
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17667

  # I don't care what base R prints this as, this is:
  # "1969-12-31T23:59:59.5"
  x <- .POSIXct(-0.5, "UTC")

  # This is
  # "1969-12-31T22:59:59.5"
  y <- .POSIXct(-3600.5, "UTC")

  expect_equal(warp_group(x, "minute"), -1)
  expect_equal(warp_group(y, "minute"), -61)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "minute")

test_that("can warp_group() by minute with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "minute"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "minute"), 525600L)
})

# ------------------------------------------------------------------------------
# warp_group(<Date>, by = "second")

test_that("can warp_group() by second with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_group(x, "second"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_group(x, "second"), 86400)

  x <- as.Date("1971-01-01")
  expect_identical(warp_group(x, "second"), 60 * 60 * 24 * 365)
})

test_that("can warp_group() by second with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_group(x, "second"), -86400)

  x <- as.Date("1969-12-30")
  expect_identical(warp_group(x, "second"), -86400 * 2L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_group(x, "second", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 seconds before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "second", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "second", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_group(x, "second"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_group(x, "second"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_group(x, "second"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_group(x, by = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_group(x, by = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_group(x, by = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_group(x, by = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_group(x, by = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_group(x, by = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_group(x, by = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_group(x, by = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_group(x, by = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_group(x, by = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_group(x, by = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_group(x, by = "second"), 0)
  expect_identical(warp_group(y, by = "second"), 0)
})

test_that("can handle second values larger than max int value", {
  x <- as.Date("2100-01-01")
  expect_equal(warp_group(x, "second"), 4102444800)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXct>, by = "second")

test_that("can warp_group() by second with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_group(x, "second"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_group(x, "second"), 86400)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_group(x, "second"), 60 * 60 * 24 * 365)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2880 seconds from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1440 seconds from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 seconds from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1440 seconds from epoch
test_that("can warp_group() by second with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second"), -172800)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second"), -86401)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second"), -86400)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second"), 86399)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second"), 86400)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second"), -(60 * 60 * 24 * 365))
})

test_that("can warp_group() by second with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), -86400)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), 0)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), 3600)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), 86399)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), 86400)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_group(x, "second", origin = origin), 172800 - 1)
})

test_that("can warp_group() by second with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), -172800)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), -86401)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), -86400)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), 86399)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_group(x, "second", origin = origin), 86400)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_group(x, "second"), NA)

  expect_identical(warp_group(x, "second"), 31536000)
  expect_identical(warp_group(x, "second", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_group(x, "second", origin = origin1), NA)

  expect_identical(warp_group(x, "second", origin = origin1), 0)
  expect_identical(warp_group(x, "second", origin = origin2), -31536000)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours behind UTC, or 300 seconds
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_group(x, "second", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_group(x_with_tz, "second", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_group(x, "second", origin = origin)),
      14400 # 4 hr * 60 min * 60 sec
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "second"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "second"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_group(x, "second"), NA_real_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_identical(warp_group(x, by = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_group(x, by = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_group(x, by = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  origin <- as.POSIXct("1970-01-01 00:00:01", tz = "UTC")

  expect_identical(warp_group(x, by = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_identical(warp_group(x, by = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_identical(warp_group(x, by = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  expect_identical(warp_group(x, by = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_group(x, by = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_group(x, by = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:00:01", tz = "UTC")

  expect_equal(warp_group(x, by = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_group(x, by = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_group(x, by = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle fractional seconds before the epoch correctly", {
  # Base R printing is wrong, because as.POSIXlt() is wrong
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17667

  # I don't care what base R prints this as, this is:
  # "1969-12-31T23:59:59.5"
  x <- .POSIXct(-0.5, "UTC")

  # This is
  # "1969-12-31T22:59:59.5"
  y <- .POSIXct(-3600.5, "UTC")

  expect_equal(warp_group(x, "second"), -1)
  expect_equal(warp_group(y, "second"), -3601)
})

test_that("can handle second values larger than max int value", {
  x <- as.POSIXct("2100-01-01", "UTC")
  expect_equal(warp_group(x, "second"), 4102444800)
})

# ------------------------------------------------------------------------------
# warp_group(<POSIXlt>, by = "second")

test_that("can warp_group() by second with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "second"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_group(x, "second"), 31536000)
})

# ------------------------------------------------------------------------------
# warp_group() misc

test_that("`x` is validated", {
  expect_error(warp_group(1), "must inherit from")
})

test_that("`origin` is validated", {
  expect_error(warp_group(new_date(0), origin = 1), "must inherit from")
  expect_error(warp_group(new_date(0), origin = new_date(c(0, 1))), "size 1, not 2")

  expect_error(warp_group(new_date(0), by = "year", origin = new_date(NA_real_)), "cannot be `NA`")
  expect_error(warp_group(new_date(0), by = "month", origin = new_date(NA_real_)), "cannot be `NA`")
})

test_that("`every` is validated", {
  expect_error(warp_group(new_date(0), every = 0), "greater than 0, not 0")
  expect_error(warp_group(new_date(0), every = -1), "greater than 0, not -1")
  expect_error(warp_group(new_date(0), every = "x"), "integer-ish, not character")
  expect_error(warp_group(new_date(0), every = c(1, 1)), "size 1, not 2")
  expect_error(warp_group(new_date(0), every = integer()), "size 1, not 0")
})

test_that("`by` is validated", {
  expect_error(warp_group(new_date(0), by = 1), "single string")
  expect_error(warp_group(new_date(0), by = c("x", "y")), "single string")
  expect_error(warp_group(new_date(0), by = "yr"), "Unknown `by` value 'yr'")
})

