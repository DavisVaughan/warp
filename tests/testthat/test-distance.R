# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "year")

test_that("can warp_distance() by year with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "year"), 0)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "year"), 1)
})

test_that("can warp_distance() by year with 'negative' Dates", {
  x <- as.Date("1969-01-01")
  expect_identical(warp_distance(x, "year"), -1)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "year", origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "year", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "year"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "year"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "year"), NA_real_)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  expect_equal(warp_distance(x, every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  origin <- as.Date("1971-01-01")

  expect_equal(warp_distance(x, every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  y <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(y), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "year"), numeric())
  expect_equal(warp_distance(x, by = "year", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "year"), numeric())
  expect_equal(warp_distance(x, by = "year", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "year")

test_that("can warp_distance() by year with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "year"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "year"), 1)
})

test_that("can warp_distance() by year with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "year"), -1)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "year"), -1)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "year"), -2)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "year"), NA)

  expect_identical(warp_distance(x, "year"), 1)
  expect_identical(warp_distance(x, "year", x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "year", origin1), NA)

  expect_identical(warp_distance(x, "year", origin = origin1), 0)
  expect_identical(warp_distance(x, "year", origin = origin2), -1)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "year", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "year", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "year", origin)),
      0
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "year"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "year"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "year"), NA_real_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  expect_equal(warp_distance(x, every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  origin <- as.Date("1971-01-01")

  expect_equal(warp_distance(x, every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_equal(warp_distance(x, every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "year")

test_that("can warp_distance() by year with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "year"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "year"), 1)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "quarter")

# This uses `by = "month"` with `every = every * 3`, so just do a basic test

test_that("can warp_distance() by quarter with Date", {
  x <- as.Date(c("1970-01-01", "1970-03-31", "1970-04-01"))

  expect_identical(warp_distance(x, "quarter"), c(0, 0, 1))
})

test_that("can adjust the origin at the month level", {
  origin <- as.Date("1970-02-01")
  x <- as.Date(c("1970-01-01", "1970-04-30", "1970-05-01"))

  expect_identical(warp_distance(x, "quarter", origin = origin), c(-1, 0, 1))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "quarter")

test_that("can warp_distance() by quarter with POSIXct", {
  x <- as.POSIXct(c("1970-01-01 00:00:00", "1970-03-31 23:59:59", "1970-04-01 00:00:00"), "UTC")

  expect_identical(warp_distance(x, "quarter"), c(0, 0, 1))
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "month")

test_that("can warp_distance() by month with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "month"), 0)

  x <- as.Date("1970-02-01")
  expect_identical(warp_distance(x, "month"), 1)

  x <- as.Date("1971-02-01")
  expect_identical(warp_distance(x, "month"), 13)
})

test_that("can warp_distance() by month with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "month"), -1)

  x <- as.Date("1968-11-30")
  expect_identical(warp_distance(x, "month"), -14)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "month", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "month", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "month"), 0)

  x <- structure(31L, class = "Date")
  expect_identical(warp_distance(x, "month"), 1)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  expect_equal(warp_distance(x, by = "month", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "month", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "month", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  origin <- as.Date("1970-02-01")

  expect_equal(warp_distance(x, by = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, by = "month"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "month"), numeric())
  expect_equal(warp_distance(x, by = "month", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "month"), numeric())
  expect_equal(warp_distance(x, by = "month", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "month")

test_that("can warp_distance() by month with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "month"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "month"), 12)
})

test_that("can warp_distance() by month with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "month"), -1)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "month"), -12)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "month"), -13)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "month"), NA)

  expect_identical(warp_distance(x, "month"), 12)
  expect_identical(warp_distance(x, "month", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "month", origin = origin1), NA)

  expect_identical(warp_distance(x, "month", origin = origin1), 0)
  expect_identical(warp_distance(x, "month", origin = origin2), -12)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "month", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "month", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "month", origin = origin)),
      0
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "month"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "month"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "month"), NA_real_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  expect_equal(warp_distance(x, by = "month", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "month", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "month", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  origin <- as.Date("1970-02-01")

  expect_equal(warp_distance(x, by = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1970-02-01", tz = "America/New_York")

  expect_equal(warp_distance(x, by = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "month")

test_that("can warp_distance() by month with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "month"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "month"), 12)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "week")

# This uses `by = "day"` with `every = every * 7`, so just do a basic test

test_that("can warp_distance() by week with Date", {
  x <- as.Date(c("1969-12-24", "1969-12-25", "1970-01-01", "1970-01-07", "1970-01-08"))

  expect_identical(warp_distance(x, "week"), c(-2, -1, 0, 0, 1))
})

test_that("can adjust the origin at the day level", {
  origin <- as.Date("1970-01-02")
  x <- as.Date(c("1969-12-24", "1969-12-25", "1970-01-01", "1970-01-07", "1970-01-08"))

  expect_identical(warp_distance(x, "week", origin = origin), c(-2, -2, -1, 0, 0))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "week")

test_that("can warp_distance() by week with POSIXct", {
  x <- as.POSIXct(c("1969-12-24", "1969-12-25", "1970-01-01", "1970-01-07", "1970-01-08"), "UTC")

  expect_identical(warp_distance(x, "week"), c(-2, -1, 0, 0, 1))
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "day")

test_that("can warp_distance() by day with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_distance(x, "day"), 1)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "day"), 365)
})

test_that("can warp_distance() by day with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "day"), -1)

  x <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "day"), -2)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "day", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "day", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "day"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "day"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "day"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_distance(x, by = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, by = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, by = "day"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "day"), numeric())
  expect_equal(warp_distance(x, by = "day", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "day"), numeric())
  expect_equal(warp_distance(x, by = "day", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "day")

test_that("can warp_distance() by day with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 1)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 365)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2 days from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1 days from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 days from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1 days from epoch
test_that("can warp_distance() by day with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -2)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -2)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -1)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day"), 1)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -365)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day"), -366)
})

test_that("can warp_distance() by day with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), -1)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), 0)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), 1)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "day", origin = origin), 1)
})

test_that("can warp_distance() by day with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), -2)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), -2)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), 0)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "day", origin = origin), 1)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "day"), NA)

  expect_identical(warp_distance(x, "day"), 365)
  expect_identical(warp_distance(x, "day", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "day", origin = origin1), NA)

  expect_identical(warp_distance(x, "day", origin = origin1), 0)
  expect_identical(warp_distance(x, "day", origin = origin2), -365)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "day", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "day", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "day", origin = origin)),
      0
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "day"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "day"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "day"), NA_real_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_distance(x, by = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, by = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  expect_equal(warp_distance(x, by = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

  expect_identical(warp_distance(x, "day"), -1)
  expect_identical(warp_distance(y, "day"), -2)
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "day")

test_that("can warp_distance() by day with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "day"), 365)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "hour")

test_that("can warp_distance() by hour with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "hour"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_distance(x, "hour"), 24)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "hour"), 24 * 365)
})

test_that("can warp_distance() by hour with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "hour"), -24)

  x <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "hour"), -48)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "hour", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "hour", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "hour", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "hour"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "hour"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "hour"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_distance(x, by = "hour", every = 48L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "hour", every = 72L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 96L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "hour", every = 48L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 72L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "hour", every = 96L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, by = "hour", every = 48L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "hour", every = 72L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 96L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "hour", every = 48L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 72L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "hour", every = 96L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 02:24:00 UTC"
  # structure(.1 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.1, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, by = "hour"), 0)
  expect_identical(warp_distance(y, by = "hour"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "hour"), numeric())
  expect_equal(warp_distance(x, by = "hour", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "hour"), numeric())
  expect_equal(warp_distance(x, by = "hour", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "hour")

test_that("can warp_distance() by hour with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 24)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 24 * 365)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -48 hours from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -24 hours from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 hours from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 24 hours from epoch
test_that("can warp_distance() by hour with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -48)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -25)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -24)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -23)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 23)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), 24)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -8760)

  x <- as.POSIXct("1968-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour"), -8761)
})

test_that("can warp_distance() by hour with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), -24)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), 0)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), 1)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), 23)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), 24)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "hour", origin = origin), 47)
})

test_that("can warp_distance() by hour with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), -48)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), -25)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), -24)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), 23)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "hour", origin = origin), 24)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "hour"), NA)

  expect_identical(warp_distance(x, "hour"), 8760)
  expect_identical(warp_distance(x, "hour", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "hour", origin = origin1), NA)

  expect_identical(warp_distance(x, "hour", origin = origin1), 0)
  expect_identical(warp_distance(x, "hour", origin = origin2), -8760)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "hour", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "hour", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "hour", origin = origin)),
      4
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "hour"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "hour"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "hour"), NA_real_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_distance(x, by = "hour", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "hour", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, by = "hour", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "hour", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  expect_equal(warp_distance(x, by = "hour", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "hour", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")

  expect_equal(warp_distance(x, by = "hour", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "hour", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "hour", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

  expect_identical(warp_distance(x, "hour"), -1)
  expect_identical(warp_distance(y, "hour"), -2)
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "hour")

test_that("can warp_distance() by hour with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "hour"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "hour"), 8760)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "minute")

test_that("can warp_distance() by minute with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "minute"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_distance(x, "minute"), 1440)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "minute"), 60 * 24 * 365)
})

test_that("can warp_distance() by minute with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "minute"), -1440)

  x <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "minute"), -1440 * 2)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "minute", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 minutes before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "minute", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "minute", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "minute"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "minute"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "minute"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_distance(x, by = "minute", every = 2880L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4320L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 5760L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "minute", every = 2880L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4320L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "minute", every = 5760L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, by = "minute", every = 2880L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4320L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 5760L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "minute", every = 2880L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4320L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "minute", every = 5760L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, by = "minute"), 0)
  expect_identical(warp_distance(y, by = "minute"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "minute"), numeric())
  expect_equal(warp_distance(x, by = "minute", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "minute"), numeric())
  expect_equal(warp_distance(x, by = "minute", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "minute")

test_that("can warp_distance() by minute with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 1440)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 60 * 24 * 365)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00) = -2880 minutes from epoch
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00) = -1440 minutes from epoch
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00) = 0 minutes from epoch
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00) = 1440 minutes from epoch
test_that("can warp_distance() by minute with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), -2880)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), -1441)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), -1440)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 1439)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), 1440)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute"), -(60 * 24 * 365))
})

test_that("can warp_distance() by minute with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), -1440)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), 0)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), 60)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), 1439)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), 1440)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "minute", origin = origin), 2880 - 1)
})

test_that("can warp_distance() by minute with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), -2880)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), -1441)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), -1440)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), 1439)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "minute", origin = origin), 1440)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "minute"), NA)

  expect_identical(warp_distance(x, "minute"), 525600)
  expect_identical(warp_distance(x, "minute", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "minute", origin = origin1), NA)

  expect_identical(warp_distance(x, "minute", origin = origin1), 0)
  expect_identical(warp_distance(x, "minute", origin = origin2), -525600)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours behind UTC, or 300 minutes
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "minute", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "minute", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "minute", origin = origin)),
      240 # 4 hr * 60 min
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "minute"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "minute"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "minute"), NA_real_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_equal(warp_distance(x, by = "minute", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "minute", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, by = "minute", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "minute", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  expect_equal(warp_distance(x, by = "minute", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "minute", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:01:00", tz = "UTC")

  expect_equal(warp_distance(x, by = "minute", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "minute", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "minute", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

  expect_identical(warp_distance(x, "minute"), -1)
  expect_identical(warp_distance(y, "minute"), -61)
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "minute")

test_that("can warp_distance() by minute with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "minute"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "minute"), 525600)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "second")

test_that("can warp_distance() by second with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "second"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_distance(x, "second"), 86400)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "second"), 60 * 60 * 24 * 365)
})

test_that("can warp_distance() by second with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "second"), -86400)

  x <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "second"), -86400 * 2L)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "second", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 seconds before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "second", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "second", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "second"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "second"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "second"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_distance(x, by = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, by = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, by = "second"), 0)
  expect_identical(warp_distance(y, by = "second"), 0)
})

test_that("can handle second values larger than max int value", {
  x <- as.Date("2100-01-01")
  expect_equal(warp_distance(x, "second"), 4102444800)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "second"), numeric())
  expect_equal(warp_distance(x, by = "second", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "second"), numeric())
  expect_equal(warp_distance(x, by = "second", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "second")

test_that("can warp_distance() by second with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 86400)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 60 * 60 * 24 * 365)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00)
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00)
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00)
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00)
test_that("can warp_distance() by second with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second"), -172800)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second"), -86401)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second"), -86400)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 86399)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second"), 86400)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second"), -(60 * 60 * 24 * 365))
})

test_that("can warp_distance() by second with 'negative' POSIXct and different UTC origins", {
  origin <- as.POSIXct("1969-12-31", tz = "UTC")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), -86400)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), -1)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), 0)

  x <- as.POSIXct("1969-12-31 01:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), 3600)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), 86399)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), 86400)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "second", origin = origin), 172800 - 1)
})

test_that("can warp_distance() by second with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), -172800)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), -86401)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), -86400)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), -1)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), 86399)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "second", origin = origin), 86400)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "second"), NA)

  expect_identical(warp_distance(x, "second"), 31536000)
  expect_identical(warp_distance(x, "second", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "second", origin = origin1), NA)

  expect_identical(warp_distance(x, "second", origin = origin1), 0)
  expect_identical(warp_distance(x, "second", origin = origin2), -31536000)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours behind UTC, or 300 seconds
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "second", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "second", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "second", origin = origin)),
      14400 # 4 hr * 60 min * 60 sec
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "second"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "second"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "second"), NA_real_)
})

test_that("can handle `every` with default origin - integer POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  x <- structure(as.integer(unclass(x)), tzone = "UTC", class = class(x))

  expect_identical(warp_distance(x, by = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, by = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, by = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_identical(warp_distance(x, by = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_identical(warp_distance(x, by = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_identical(warp_distance(x, by = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  expect_identical(warp_distance(x, by = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, by = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, by = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:00:01", tz = "UTC")

  expect_equal(warp_distance(x, by = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

  expect_equal(warp_distance(x, "second"), -1)
  expect_equal(warp_distance(y, "second"), -3601)
})

test_that("can handle second values larger than max int value", {
  x <- as.POSIXct("2100-01-01", "UTC")
  expect_equal(warp_distance(x, "second"), 4102444800)
})

test_that("`origin` could have fractional components (ignore them) - integer POSIXct", {
  origin <- as.POSIXct("1969-12-31 23:59:59.998", "UTC")
  x <- structure(c(-2L, -1L, 0L, 1L), tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_identical(warp_distance(x, "second", origin = origin), c(-1, 0, 1, 2))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "second")

test_that("can warp_distance() by second with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "second"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "second"), 31536000)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, by = "millisecond")

test_that("can warp_distance() by millisecond with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "millisecond"), 0)

  x <- as.Date("1970-01-02")
  expect_identical(warp_distance(x, "millisecond"), 86400 * 1000)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "millisecond"), 60 * 60 * 24 * 365 * 1000)
})

test_that("can warp_distance() by millisecond with 'negative' Dates", {
  x <- as.Date("1969-12-31")
  expect_identical(warp_distance(x, "millisecond"), -86400 * 1000)

  x <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "millisecond"), -86400 * 2 * 1000)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "millisecond", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  # America/New_York is 5 milliseconds before UTC
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "millisecond", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "millisecond", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "millisecond"), 0)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, class = "Date")
  expect_identical(warp_distance(x, "millisecond"), NA_real_)

  x <- structure(NA_integer_, class = "Date")
  expect_identical(warp_distance(x, "millisecond"), NA_real_)
})

test_that("can handle `every` with default origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  expect_equal(warp_distance(x, by = "millisecond", every = 172800 * 1000), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 259200 * 1000), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 345600 * 1000), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "millisecond", every = 172800 * 1000, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 259200 * 1000, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "millisecond", every = 345600 * 1000, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, by = "millisecond", every = 172800 * 1000), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 259200 * 1000), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 345600 * 1000), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, by = "millisecond", every = 172800 * 1000, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 259200 * 1000, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "millisecond", every = 345600 * 1000, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01T00:00:00.432 UTC"
  # structure(.000005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.000005, class = "Date")

  # "1969-12-31T23:59:59.568 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.000005, class = "Date")

  # "1970-01-02T00:00:00.432 UTC"
  z <- structure(1.000005, class = "Date")

  expect_identical(warp_distance(x, by = "millisecond"), 0)
  expect_identical(warp_distance(y, by = "millisecond"), 0)
  expect_identical(warp_distance(z, by = "millisecond"), 86400000)
})

test_that("can handle millisecond values larger than max int value", {
  x <- as.Date("2100-01-01")
  expect_equal(warp_distance(x, "millisecond"), 4102444800 * 1000)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, by = "millisecond"), numeric())
  expect_equal(warp_distance(x, by = "millisecond", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, by = "millisecond"), numeric())
  expect_equal(warp_distance(x, by = "millisecond", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, by = "millisecond")

test_that("can warp_distance() by millisecond with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 0)

  x <- as.POSIXct("1970-01-02", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 86400 * 1000)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 60 * 60 * 24 * 365 * 1000)
})

# In terms of inclusion/exclusion, we define the cutoffs like:
# [1969-12-30 00:00:00 -> 1969-12-31 00:00:00)
# [1969-12-31 00:00:00 -> 1970-01-01 00:00:00)
# [1970-01-01 00:00:00 -> 1970-01-02 00:00:00)
# [1970-01-02 00:00:00 -> 1970-01-03 00:00:00)
test_that("can warp_distance() by millisecond with 'negative' POSIXct", {
  x <- as.POSIXct("1969-12-30 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), -172800 * 1000)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), -86401 * 1000)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), -86400 * 1000)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), -1 * 1000)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 86399 * 1000)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), 86400 * 1000)

  x <- as.POSIXct("1969-01-01 00:00:00", tz = "UTC")
  expect_identical(warp_distance(x, "millisecond"), -(60 * 60 * 24 * 365) * 1000)
})

test_that("can warp_distance() by millisecond with 'negative' POSIXct and non-UTC origins", {
  origin <- as.POSIXct("1970-01-01", tz = "America/New_York")

  x <- as.POSIXct("1969-12-30 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), -172800 * 1000)

  x <- as.POSIXct("1969-12-30 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), -86401 * 1000)

  x <- as.POSIXct("1969-12-31 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), -86400 * 1000)

  x <- as.POSIXct("1969-12-31 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), -1 * 1000)

  x <- as.POSIXct("1970-01-01 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), 0)

  x <- as.POSIXct("1970-01-01 23:59:59", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), 86399 * 1000)

  x <- as.POSIXct("1970-01-02 00:00:00", tz = "America/New_York")
  expect_identical(warp_distance(x, "millisecond", origin = origin), 86400 * 1000)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "millisecond"), NA)

  expect_identical(warp_distance(x, "millisecond"), 31536000 * 1000)
  expect_identical(warp_distance(x, "millisecond", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "millisecond", origin = origin1), NA)

  expect_identical(warp_distance(x, "millisecond", origin = origin1), 0)
  expect_identical(warp_distance(x, "millisecond", origin = origin2), -31536000 * 1000)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  # America/New_York is 5 hours behind UTC, or 300 milliseconds
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "millisecond", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "millisecond", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01-01 04:00:00
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "millisecond", origin = origin)),
      14400 * 1000 # 4 hr * 60 min * 60 sec * 1000 milliseconds
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "millisecond"), -1 * 1000)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "millisecond"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "millisecond"), NA_real_)
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:59.997", "1969-12-31 23:59:59.998",
    "1969-12-31 23:59:59.999", "1970-01-01 00:00:00.000",
    "1970-01-01 00:00:00.001", "1970-01-01 00:00:00.002",
    "1970-01-01 00:00:00.003"
  ), tz = "UTC")

  expect_identical(warp_distance(x, by = "millisecond", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, by = "millisecond", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, by = "millisecond", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:59.997", "1969-12-31 23:59:59.998",
    "1969-12-31 23:59:59.999", "1970-01-01 00:00:00.000",
    "1970-01-01 00:00:00.001", "1970-01-01 00:00:00.002",
    "1970-01-01 00:00:00.003"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:00:00.001", tz = "UTC")

  expect_equal(warp_distance(x, by = "millisecond", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, by = "millisecond", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, by = "millisecond", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle fractional pieces with decimilliseconds correctly", {
  x <- as.POSIXct("1969-12-31 23:59:59.9989", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -2)

  x <- as.POSIXct("1969-12-31 23:59:59.9995", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -1)

  x <- as.POSIXct("1969-12-31 23:59:59.9999", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00.0000", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.0009", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.0010", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 1)

  x <- as.POSIXct("1970-01-01 00:00:00.0011", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 1)
})

test_that("can handle fractional pieces with centimilliseconds correctly", {
  # Base R is going to print these wrong, use {nanotime} if you need to
  # print them correctly
  x <- as.POSIXct("1969-12-31 23:59:59.99809", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -2)

  x <- as.POSIXct("1969-12-31 23:59:59.99905", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -1)

  x <- as.POSIXct("1969-12-31 23:59:59.99909", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00.00000", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.00009", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.00100", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 1)

  x <- as.POSIXct("1970-01-01 00:00:00.00101", "UTC")
  expect_identical(warp_distance(x, by = "millisecond"), 1)
})

test_that("`origin` could have floating point error values that need guarding - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:59.997", "1969-12-31 23:59:59.998",
    "1969-12-31 23:59:59.999", "1970-01-01 00:00:00.000",
    "1970-01-01 00:00:00.001", "1970-01-01 00:00:00.002",
    "1970-01-01 00:00:00.003"
  ), tz = "UTC")

  # We know this one can't be represented exactly in floating point
  origin <- as.POSIXct("1969-12-31 23:59:59.998", "UTC")

  expect_identical(warp_distance(x, by = "millisecond", every = 1L, origin = origin), c(-1, 0, 1, 2, 3, 4, 5))
  expect_identical(warp_distance(x, by = "millisecond", every = 2L, origin = origin), c(-1, 0, 0, 1, 1, 2, 2))
})

test_that("`origin` could have floating point error values that need guarding - integer POSIXct", {
  # "1970-01-01 00:00:00 UTC" "1969-12-31 23:59:59 UTC"
  x <- structure(c(0L, -1L), tzone = "UTC", class = c("POSIXct", "POSIXt"))

  # We know this one can't be represented exactly in floating point
  origin <- as.POSIXct("1969-12-31 23:59:59.998", "UTC")

  expect_identical(warp_distance(x, by = "millisecond", every = 1L, origin = origin), c(2, -998))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, by = "millisecond")

test_that("can warp_distance() by millisecond with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "millisecond"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "millisecond"), 31536000 * 1000)
})

# ------------------------------------------------------------------------------
# warp_distance() misc

test_that("`x` is validated", {
  expect_error(warp_distance(1), "must inherit from")
})

test_that("`origin` is validated", {
  expect_error(warp_distance(new_date(0), origin = 1), "must inherit from")
  expect_error(warp_distance(new_date(0), origin = new_date(c(0, 1))), "size 1, not 2")

  expect_error(warp_distance(new_date(0), by = "year", origin = new_date(NA_real_)), "cannot be `NA`")
  expect_error(warp_distance(new_date(0), by = "month", origin = new_date(NA_real_)), "cannot be `NA`")
})

test_that("`every` is validated", {
  expect_error(warp_distance(new_date(0), every = 0), "greater than 0, not 0")
  expect_error(warp_distance(new_date(0), every = -1), "greater than 0, not -1")
  expect_error(warp_distance(new_date(0), every = "x"), "integer-ish, not character")
  expect_error(warp_distance(new_date(0), every = c(1, 1)), "size 1, not 2")
  expect_error(warp_distance(new_date(0), every = integer()), "size 1, not 0")
  expect_error(warp_distance(new_date(0), every = NA_integer_), "`every` must not be `NA`")
})

test_that("`by` is validated", {
  expect_error(warp_distance(new_date(0), by = 1), "single string")
  expect_error(warp_distance(new_date(0), by = c("x", "y")), "single string")
  expect_error(warp_distance(new_date(0), by = "yr"), "Unknown `by` value 'yr'")
})

