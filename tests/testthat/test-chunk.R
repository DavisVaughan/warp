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
      warp_chunk(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "year", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_chunk(x, "year"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_chunk(x, "year"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_chunk(x, "year"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  expect_equal(warp_chunk(x, every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  origin <- as.Date("1971-01-01")

  expect_equal(warp_chunk(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
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

  expect_identical(warp_chunk(x, "year", origin = origin1), 0L)
  expect_identical(warp_chunk(x, "year", origin = origin2), -1L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "year", origin = origin)
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

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "year"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "year"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  expect_equal(warp_chunk(x, every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  origin <- as.Date("1971-01-01")

  expect_equal(warp_chunk(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_equal(warp_chunk(x, every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
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

  expect_identical(warp_chunk(x, "month", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "month", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- structure(31L, class = "Date")
  expect_identical(warp_chunk(x, "month"), 1L)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  expect_equal(warp_chunk(x, by = "month", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  origin <- as.Date("1970-02-01")

  expect_equal(warp_chunk(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
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
  expect_identical(warp_chunk(x, "month", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_chunk(x, "month", origin = origin1), NA)

  expect_identical(warp_chunk(x, "month", origin = origin1), 0L)
  expect_identical(warp_chunk(x, "month", origin = origin2), -12L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "month", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_chunk(x, "month", origin = origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "month"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "month"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "month"), NA_integer_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  expect_equal(warp_chunk(x, by = "month", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  origin <- as.Date("1970-02-01")

  expect_equal(warp_chunk(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1970-02-01", tz = "America/New_York")

  expect_equal(warp_chunk(x, by = "month", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "month", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "month", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXlt>, by = "month")

test_that("can warp_chunk() by month with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "month"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "month"), 12L)
})

# ------------------------------------------------------------------------------
# warp_chunk(<Date>, by = "day")

test_that("can warp_chunk() by day with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_chunk(x, "day"), 0L)

  x <- as.Date("1970-01-02")
  expect_identical(warp_chunk(x, "day"), 1L)

  x <- as.Date("1971-01-01")
  expect_identical(warp_chunk(x, "day"), 365L)
})

test_that("can warp_chunk() by day with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_chunk(x, "day"), -1L)

  x <- as.Date("1969-12-30")
  expect_identical(warp_chunk(x, "day"), -2L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_chunk(x, "day", origin = origin), 0L)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "day", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_chunk(x, "day"), 0L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_chunk(x, "day"), NA_integer_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_chunk(x, "day"), NA_integer_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_chunk(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_chunk(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_chunk(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_chunk(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXct>, by = "day")

test_that("can warp_chunk() by day with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 0L)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 1L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 365L)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2 days from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1 days from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 days from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1 days from epoch
test_that("can warp_chunk() by day with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -2L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -2L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -1L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 0L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), 1L)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -365L)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day"), -366L)
})

test_that("can warp_chunk() by day with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), 1L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_chunk(x, "day", origin = origin), 1L)
})

test_that("can warp_chunk() by day with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), -2L)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), -2L)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), -1L)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), 0L)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_chunk(x, "day", origin = origin), 1L)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_chunk(x, "day"), NA)

  expect_identical(warp_chunk(x, "day"), 365L)
  expect_identical(warp_chunk(x, "day", origin = x), 0L)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_chunk(x, "day", origin = origin1), NA)

  expect_identical(warp_chunk(x, "day", origin = origin1), 0L)
  expect_identical(warp_chunk(x, "day", origin = origin2), -365L)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_chunk(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_chunk(x_with_tz, "day", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_chunk(x, "day", origin = origin)),
      0L
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "day"), -1L)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "day"), NA_integer_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_chunk(x, "day"), NA_integer_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_chunk(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
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

  expect_equal(warp_chunk(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  expect_equal(warp_chunk(x, by = "day", every = 2L), c(-2L, -1L, -1L, 0L, 0L, 1L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L), c(-1L, -1L, -1L, 0L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 4L), c(-1L, -1L, -1L, 0L, 0L, 0L, 0L))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_chunk(x, by = "day", every = 2L, origin = origin), c(-2L, -2L, -1L, -1L, 0L, 0L, 1L))
  expect_equal(warp_chunk(x, by = "day", every = 3L, origin = origin), c(-2L, -1L, -1L, -1L, 0L, 0L, 0L))
  expect_equal(warp_chunk(x, by = "day", every = 4L, origin = origin), c(-1L, -1L, -1L, -1L, 0L, 0L, 0L))
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

  expect_equal(warp_chunk(x, "day"), -1)
  expect_equal(warp_chunk(y, "day"), -2)
})

# ------------------------------------------------------------------------------
# warp_chunk(<POSIXlt>, by = "day")

test_that("can warp_chunk() by day with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "day"), 0L)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_chunk(x, "day"), 365L)
})

# ------------------------------------------------------------------------------
# warp_chunk() misc

test_that("`x` is validated", {
  expect_error(warp_chunk(1), "must inherit from")
})

test_that("`origin` is validated", {
  expect_error(warp_chunk(new_date(0), origin = 1), "must inherit from")
  expect_error(warp_chunk(new_date(0), origin = new_date(c(0, 1))), "size 1, not 2")

  expect_error(warp_chunk(new_date(0), by = "year", origin = new_date(NA_real_)), "cannot be `NA`")
  expect_error(warp_chunk(new_date(0), by = "month", origin = new_date(NA_real_)), "cannot be `NA`")
})

test_that("`every` is validated", {
  expect_error(warp_chunk(new_date(0), every = 0), "greater than 0, not 0")
  expect_error(warp_chunk(new_date(0), every = -1), "greater than 0, not -1")
  expect_error(warp_chunk(new_date(0), every = "x"), "integer-ish, not character")
  expect_error(warp_chunk(new_date(0), every = c(1, 1)), "size 1, not 2")
  expect_error(warp_chunk(new_date(0), every = integer()), "size 1, not 0")
})

test_that("`by` is validated", {
  expect_error(warp_chunk(new_date(0), by = 1), "single string")
  expect_error(warp_chunk(new_date(0), by = c("x", "y")), "single string")
  expect_error(warp_chunk(new_date(0), by = "yr"), "Unknown `by` value 'yr'")
})

