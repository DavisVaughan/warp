# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "year")

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

  expect_identical(warp_distance(x, "year", origin = origin), 0)
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

  expect_equal(warp_distance(x, period = "year", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "year", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "year", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ))

  origin <- as.Date("1971-01-01")

  expect_equal(warp_distance(x, period = "year", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "year", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "year", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  y <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(y, period = "year"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "year"), numeric())
  expect_equal(warp_distance(x, period = "year", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "year"), numeric())
  expect_equal(warp_distance(x, period = "year", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "year")

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
  expect_identical(warp_distance(x, "year", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "year", origin = origin1), NA)

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
      expect_warning(warp_distance(x, "year", origin = origin)),
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

  expect_equal(warp_distance(x, period = "year", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "year", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "year", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "UTC")

  origin <- as.Date("1971-01-01")

  expect_equal(warp_distance(x, period = "year", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "year", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "year", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1967-01-01", "1968-01-01",
    "1969-01-01", "1970-01-01",
    "1971-01-01", "1972-01-01",
    "1973-01-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_equal(warp_distance(x, period = "year", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "year", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "year", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "year"),
    warp_distance(y, period = "year")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "year")

test_that("can warp_distance() by year with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "year"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "year"), 1)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "quarter")

# This uses `period = "month"` with `every = every * 3`, so just do a basic test

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
# warp_distance(<POSIXct>, period = "quarter")

test_that("can warp_distance() by quarter with POSIXct", {
  x <- as.POSIXct(c("1970-01-01 00:00:00", "1970-03-31 23:59:59", "1970-04-01 00:00:00"), "UTC")

  expect_identical(warp_distance(x, "quarter"), c(0, 0, 1))
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "month")

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

  expect_equal(warp_distance(x, period = "month", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "month", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "month", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ))

  origin <- as.Date("1970-02-01")

  expect_equal(warp_distance(x, period = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, period = "month"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "month"), numeric())
  expect_equal(warp_distance(x, period = "month", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "month"), numeric())
  expect_equal(warp_distance(x, period = "month", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "month")

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

  expect_equal(warp_distance(x, period = "month", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "month", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "month", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "UTC")

  origin <- as.Date("1970-02-01")

  expect_equal(warp_distance(x, period = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1969-10-01", "1969-11-01",
    "1969-12-01", "1970-01-01",
    "1970-02-01", "1970-03-01",
    "1970-04-01"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1970-02-01", tz = "America/New_York")

  expect_equal(warp_distance(x, period = "month", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "month", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "month", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "month"),
    warp_distance(y, period = "month")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "month")

test_that("can warp_distance() by month with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "month"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "month"), 12)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "yweek")

test_that("can warp_distance() by yweek with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "yweek"), 0)

  x <- as.Date("1970-01-08")
  expect_identical(warp_distance(x, "yweek"), 1)

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "yweek"), 53)
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "yweek", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "yweek", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "yweek", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "yweek"), 0)

  x <- structure(31L, class = "Date")
  expect_identical(warp_distance(x, "yweek"), 4)
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1969-12-02",
    "1969-12-09", "1969-12-16",
    "1969-12-23", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-08", "1970-01-15",
    "1970-01-22"
  ))

  expect_equal(warp_distance(x, period = "yweek", every = 2L), c(-4, -3, -3, -2, -2, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 3L), c(-3, -2, -2, -2, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 4L), c(-3, -2, -2, -2, -2, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1969-12-02",
    "1969-12-09", "1969-12-16",
    "1969-12-23", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-06", "1970-01-07",
    "1970-01-08", "1970-01-15",
    "1970-01-22"
  ))

  origin <- as.Date("1970-01-08")

  expect_equal(warp_distance(x, period = "yweek", every = 2L, origin = origin), c(-4, -4, -3, -3, -2, -2, -2, -2, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 3L, origin = origin), c(-3, -3, -2, -2, -2, -1, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "yweek", every = 4L, origin = origin), c(-3, -3, -2, -2, -2, -2, -2, -2, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, period = "yweek"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "yweek"), numeric())
  expect_equal(warp_distance(x, period = "yweek", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "yweek"), numeric())
  expect_equal(warp_distance(x, period = "yweek", every = 2), numeric())
})

test_that("going backwards in time still uses groups computed from the first of the year", {
  # The 53rd yweek of 1969
  x <- as.Date("1969-12-31")
  # The 52nd yweek of 1969
  y <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "yweek"), -1)
  expect_identical(warp_distance(y, "yweek"), -2)
})

test_that("leap years in `x` affect the yweek group", {
  origin <- as.Date("1999-03-01")

  x <- as.Date(c("1999-02-27", "1999-02-28", "1999-03-01", "1999-03-02", "1999-03-07", "1999-03-08"))
  y <- as.Date(c("2000-02-27", "2000-02-28", "2000-02-29", "2000-03-01", "2000-03-02", "2000-03-07", "2000-03-08"))
  z <- as.Date(c("2001-02-27", "2001-02-28", "2001-03-01", "2001-03-02", "2001-03-07", "2001-03-08"))

  # 1998-03-01 -> 1999-02-21 = 51 full weeks
  # [1999-02-21, 1999-02-28) = -2
  # [1999-02-28, 1999-03-01) = -1
  # [1999-03-01, 1999-03-08) = 0
  # [1999-03-08, 1999-03-15) = 1
  expect_identical(warp_distance(x, period = "yweek", every = 1, origin = origin), c(-2, -1, 0, 0, 0, 1))

  # 1999-03-01 -> 2000-02-21 = 51 full weeks
  # [2000-02-21, 2000-02-28) = 51
  # [2000-02-28, 2000-03-01) = 52
  # [2000-03-01, 2000-03-08) = 53
  # [2000-03-08, 2000-03-15) = 54
  expect_identical(warp_distance(y, period = "yweek", every = 1, origin = origin), c(51, 52, 52, 53, 53, 53, 54))

  # 2000-03-01 -> 2001-02-21 = 51 full weeks
  # [2001-02-21, 2001-02-28) = 104
  # [2001-02-28, 2001-03-01) = 105
  # [2001-03-01, 2001-03-08) = 106
  # [2001-03-08, 2001-03-15) = 107
  expect_identical(warp_distance(z, period = "yweek", every = 1, origin = origin), c(104, 105, 106, 106, 106, 107))
})

test_that("leap years in `origin` affect the yweek group", {
  origin <- as.Date("2000-03-01")

  x <- as.Date(c("1999-02-27", "1999-02-28", "1999-03-01", "1999-03-02", "1999-03-07", "1999-03-08"))
  y <- as.Date(c("2000-02-27", "2000-02-28", "2000-02-29", "2000-03-01", "2000-03-02", "2000-03-07", "2000-03-08"))
  z <- as.Date(c("2001-02-27", "2001-02-28", "2001-03-01", "2001-03-02", "2001-03-07", "2001-03-08"))

  # 1998-03-01 -> 1999-02-21 = 51 full weeks
  # [1999-02-21, 1999-02-28) = -55
  # [1999-02-28, 1999-03-01) = -54
  # [1999-03-01, 1999-03-08) = -53
  # [1999-03-08, 1999-03-15) = -52
  expect_identical(warp_distance(x, period = "yweek", every = 1, origin = origin), c(-55, -54, -53, -53, -53, -52))

  # 1999-03-01 -> 2000-02-21 = 51 full weeks
  # [2000-02-21, 2000-02-28) = -2
  # [2000-02-28, 2000-03-01) = -1
  # [2000-03-01, 2000-03-08) = 0
  # [2000-03-08, 2000-03-15) = 1
  expect_identical(warp_distance(y, period = "yweek", every = 1, origin = origin), c(-2, -1, -1, 0, 0, 0, 1))

  # 2000-03-01 -> 2001-02-21 = 51 full weeks
  # [2001-02-21, 2001-02-28) = 104
  # [2001-02-28, 2001-03-01) = 105
  # [2001-03-01, 2001-03-08) = 106
  # [2001-03-08, 2001-03-15) = 107
  expect_identical(warp_distance(z, period = "yweek", every = 1, origin = origin), c(51, 52, 53, 53, 53, 54))
})

test_that("`origin` value can be on the leap day", {
  origin <- as.Date("2000-02-29")

  # Non-leap years start that group on 02/28
  x <- as.Date(c("1999-02-27", "1999-02-28", "1999-03-01", "1999-03-02", "1999-03-07", "1999-03-08"))
  y <- as.Date(c("2000-02-27", "2000-02-28", "2000-02-29", "2000-03-01", "2000-03-02", "2000-03-07", "2000-03-08"))
  z <- as.Date(c("2001-02-27", "2001-02-28", "2001-03-01", "2001-03-02", "2001-03-07", "2001-03-08"))

  # 1998-02-28 -> 1999-02-27 = 52 full weeks
  # [1999-02-27, 1999-02-28) = -54
  # [1999-02-28, 1999-03-07) = -53
  # [1999-03-07, 1999-03-15) = -52
  expect_identical(warp_distance(x, period = "yweek", every = 1, origin = origin), c(-54, -53, -53, -53, -52, -52))

  # 1999-02-28 -> 2000-02-27 = 52 full weeks
  # [2000-02-27, 2000-02-29) = -1
  # [2000-02-29, 2000-03-07) = 0
  # [2000-03-07, 2000-03-15) = 1
  expect_identical(warp_distance(y, period = "yweek", every = 1, origin = origin), c(-1, -1, 0, 0, 0, 1, 1))

  # 2000-02-29 -> 2001-02-27 = 52 full weeks
  # [2001-02-27, 2001-02-28) = 52
  # [2001-02-28, 2001-03-07) = 53
  # [2001-03-07, 2001-03-15) = 54
  expect_identical(warp_distance(z, period = "yweek", every = 1, origin = origin), c(52, 53, 53, 53, 54, 54))
})

test_that("Ignoring the leap adjustment if before Feb 28th is required", {
  origin <- as.Date("1970-01-01")
  x <- as.Date(c("2019-12-31", "2020-01-01"))

  # if we don't ignore the leap adjustment we end up with both at 2649
  expect_identical(warp_distance(x, "yweek", origin = origin), c(2649, 2650))
})

test_that("sanity check `every`", {
  expect_error(warp_distance(as.Date("1970-01-01"), "yweek", every = 53), "is 52")
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "yweek")

test_that("can warp_distance() by yweek with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "yweek"), 0)

  x <- as.POSIXct("1970-01-08", tz = "UTC")
  expect_identical(warp_distance(x, "yweek"), 1)
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "yweek"), NA)

  expect_identical(warp_distance(x, "yweek"), 53)
  expect_identical(warp_distance(x, "yweek", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "yweek", origin = origin1), NA)

  expect_identical(warp_distance(x, "yweek", origin = origin1), 0)
  expect_identical(warp_distance(x, "yweek", origin = origin2), -53)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "yweek", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "yweek", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "yweek", origin = origin)),
      0
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "yweek"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "yweek"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "yweek"), NA_real_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1969-12-02",
    "1969-12-09", "1969-12-16",
    "1969-12-23", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-08", "1970-01-15",
    "1970-01-22"
  ), tz = "UTC")

  expect_equal(warp_distance(x, period = "yweek", every = 2L), c(-4, -3, -3, -2, -2, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 3L), c(-3, -2, -2, -2, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 4L), c(-3, -2, -2, -2, -2, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1969-12-02",
    "1969-12-09", "1969-12-16",
    "1969-12-23", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-06", "1970-01-07",
    "1970-01-08", "1970-01-15",
    "1970-01-22"
  ), tz = "UTC")

  origin <- as.Date("1970-01-08")

  expect_equal(warp_distance(x, period = "yweek", every = 2L, origin = origin), c(-4, -4, -3, -3, -2, -2, -2, -2, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 3L, origin = origin), c(-3, -3, -2, -2, -2, -1, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "yweek", every = 4L, origin = origin), c(-3, -3, -2, -2, -2, -2, -2, -2, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1969-12-02",
    "1969-12-09", "1969-12-16",
    "1969-12-23", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-06", "1970-01-07",
    "1970-01-08", "1970-01-15",
    "1970-01-22"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1970-01-08", tz = "America/New_York")

  expect_equal(warp_distance(x, period = "yweek", every = 2L, origin = origin), c(-4, -4, -3, -3, -2, -2, -2, -2, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "yweek", every = 3L, origin = origin), c(-3, -3, -2, -2, -2, -1, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "yweek", every = 4L, origin = origin), c(-3, -3, -2, -2, -2, -2, -2, -2, -1, 0, 0, 0))
})

test_that("going backwards in time still uses groups computed from the first of the year", {
  # The 53rd yweek of 1969
  x <- as.POSIXct("1969-12-31", "UTC")
  # The 52nd yweek of 1969
  y <- as.POSIXct("1969-12-30", "UTC")
  expect_identical(warp_distance(x, "yweek"), -1)
  expect_identical(warp_distance(y, "yweek"), -2)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "yweek"),
    warp_distance(y, period = "yweek")
  )
})

test_that("`origin` value can be on the leap day", {
  origin <- as.POSIXct("2000-02-29", "UTC")

  # Non-leap years start that group on 02/28
  x <- as.POSIXct(c("1999-02-27", "1999-02-28", "1999-03-01", "1999-03-02", "1999-03-07", "1999-03-08"), "UTC")
  y <- as.POSIXct(c("2000-02-27", "2000-02-28", "2000-02-29", "2000-03-01", "2000-03-02", "2000-03-07", "2000-03-08"), "UTC")
  z <- as.POSIXct(c("2001-02-27", "2001-02-28", "2001-03-01", "2001-03-02", "2001-03-07", "2001-03-08"), "UTC")

  # 1998-02-28 -> 1999-02-27 = 52 full weeks
  # [1999-02-27, 1999-02-28) = -54
  # [1999-02-28, 1999-03-07) = -53
  # [1999-03-07, 1999-03-15) = -52
  expect_identical(warp_distance(x, period = "yweek", every = 1, origin = origin), c(-54, -53, -53, -53, -52, -52))

  # 1999-02-28 -> 2000-02-27 = 52 full weeks
  # [2000-02-27, 2000-02-29) = -1
  # [2000-02-29, 2000-03-07) = 0
  # [2000-03-07, 2000-03-15) = 1
  expect_identical(warp_distance(y, period = "yweek", every = 1, origin = origin), c(-1, -1, 0, 0, 0, 1, 1))

  # 2000-02-29 -> 2001-02-27 = 52 full weeks
  # [2001-02-27, 2001-02-28) = 52
  # [2001-02-28, 2001-03-07) = 53
  # [2001-03-07, 2001-03-15) = 54
  expect_identical(warp_distance(z, period = "yweek", every = 1, origin = origin), c(52, 53, 53, 53, 54, 54))
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "yweek")

test_that("can warp_distance() by yweek with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "yweek"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "yweek"), 53)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "mweek")

# Mainly tested in `period = "mday"`

test_that("warp_distance() with mweek period works", {
  x <- as.Date("1970-01-01") + 0:500

  expect_identical(
    warp_distance(x, "mweek"),
    warp_distance(x, "mday", every = 7)
  )
})

test_that("sanity check `every`", {
  expect_error(warp_distance(new_date(0), "mweek", every = 5), "is 4")
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "week")

# Mainly tested in `period = "day"`

test_that("warp_distance() with week period works through day", {
  x <- as.Date("1970-01-01") + 0:500

  expect_identical(
    warp_distance(x, "week"),
    warp_distance(x, "day", every = 7)
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "yday")

# Mainly tested in `period = "yweek"`

test_that("warp_distance() with yday period works", {
  x <- as.Date("1970-01-01") + 0:500

  expect_identical(
    warp_distance(x, "yday", every = 7),
    warp_distance(x, "yweek")
  )
})

test_that("sanity check `every`", {
  expect_error(warp_distance(new_date(0), "yday", every = 365), "is 364")
})

test_that("can use an integer Date origin with yday", {
  origin <- structure(1L, class = "Date")
  expect_identical(warp_distance(new_date(0), "yday", origin = origin), -1)
})

test_that("integer Date origin that is NA is an error", {
  origin <- structure(NA_integer_, class = "Date")
  expect_error(warp_distance(new_date(0), "yday", origin = origin), "cannot be `NA`")
})

test_that("double Date origin that is NA / NaN / Inf is an error", {
  origin <- structure(NA_real_, class = "Date")
  expect_error(warp_distance(new_date(0), "yday", origin = origin), "must be finite")

  origin <- structure(NaN, class = "Date")
  expect_error(warp_distance(new_date(0), "yday", origin = origin), "must be finite")

  origin <- structure(Inf, class = "Date")
  expect_error(warp_distance(new_date(0), "yday", origin = origin), "must be finite")
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "mday")

test_that("can warp_distance() by mday with Date", {
  x <- as.Date("1970-01-01")
  expect_identical(warp_distance(x, "mday"), warp_distance(x, "day"))

  x <- as.Date("1970-01-08")
  expect_identical(warp_distance(x, "mday"), warp_distance(x, "day"))

  x <- as.Date("1971-01-01")
  expect_identical(warp_distance(x, "mday"), warp_distance(x, "day"))
})

test_that("Date + UTC origin does not emit a warning", {
  x <- as.Date("1971-01-01")
  origin <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_identical(warp_distance(x, "mday", origin = origin), 0)
})

test_that("Date + non-UTC origin converts with a warning", {
  x <- as.Date("1971-01-01")
  x_with_tz <- structure(unclass(x) * 86400, tzone = "America/New_York", class = c("POSIXct", "POSIXt"))
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "mday", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "mday", origin = origin)
  )
})

test_that("can use integer Dates", {
  x <- structure(0L, class = "Date")
  expect_identical(warp_distance(x, "mday"), 0)

  x <- structure(31L, class = "Date")
  expect_identical(warp_distance(x, "mday"), 31)
})

test_that("leap year borders are handled correctly", {
  x <- as.Date(c("1968-12-31", "1969-01-01"))
  origin <- as.Date("1969-01-01")

  expect_equal(warp_distance(x, "mday", every = 2, origin = origin), c(-1, 0))
})

test_that("can handle `every` with default origin", {
  x <- as.Date(c(
    "1968-12-31", "1969-01-01",
    "1969-11-30", "1969-12-01",
    "1969-12-28", "1969-12-29",
    "1969-12-30", "1969-12-31",
    "1970-01-01",
    "1970-01-02", "1970-01-03"
  ))

  expect_equal(warp_distance(x, period = "mday", every = 2L), c(-187, -186, -17, -16, -3, -2, -2, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "mday", every = 3L), c(-128, -127, -12, -11, -2, -2, -2, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "mday", every = 4L), c(-96, -95, -9, -8, -2, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.Date(c(
    "1968-12-31", "1969-01-01",
    "1969-11-30", "1969-12-01",
    "1969-12-28", "1969-12-29",
    "1969-12-30", "1969-12-31",
    "1970-01-01",
    "1970-01-02", "1970-01-03"
  ))

  origin <- as.Date("1969-12-01")

  expect_equal(warp_distance(x, period = "mday", every = 2L, origin = origin), c(-171, -170, -1, 0, 13, 14, 14, 15, 16, 16, 17))
  expect_equal(warp_distance(x, period = "mday", every = 3L, origin = origin), c(-117, -116, -1, 0, 9, 9, 9, 10, 11, 11, 11))
  expect_equal(warp_distance(x, period = "mday", every = 4L, origin = origin), c(-88, -87, -1, 0, 6, 7, 7, 7, 8, 8, 8))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, period = "mday"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "mday"), numeric())
  expect_equal(warp_distance(x, period = "mday", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "mday"), numeric())
  expect_equal(warp_distance(x, period = "mday", every = 2), numeric())
})

test_that("going backwards in time still uses groups computed from the first of the year", {
  # The 53rd mday of 1969
  x <- as.Date("1969-12-31")
  # The 52nd mday of 1969
  y <- as.Date("1969-12-30")
  expect_identical(warp_distance(x, "mday"), -1)
  expect_identical(warp_distance(y, "mday"), -2)
})

test_that("sanity check `every`", {
  expect_error(warp_distance(as.Date("1970-01-01"), "mday", every = 31), "is 30")
})

test_that("can use an integer Date origin with mday", {
  origin <- structure(-1L, class = "Date")
  expect_identical(warp_distance(new_date(0), "mday", origin = origin), 31)
})

test_that("integer Date origin that is NA is an error", {
  origin <- structure(NA_integer_, class = "Date")
  expect_error(warp_distance(new_date(0), "mday", origin = origin), "cannot be `NA`")
})

test_that("double Date origin that is NA / NaN / Inf is an error", {
  origin <- structure(NA_real_, class = "Date")
  expect_error(warp_distance(new_date(0), "mday", origin = origin), "must be finite")

  origin <- structure(NaN, class = "Date")
  expect_error(warp_distance(new_date(0), "mday", origin = origin), "must be finite")

  origin <- structure(Inf, class = "Date")
  expect_error(warp_distance(new_date(0), "mday", origin = origin), "must be finite")
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "mday")

test_that("can warp_distance() by mday with POSIXct", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  expect_identical(warp_distance(x, "mday"), warp_distance(x, "day"))

  x <- as.POSIXct("1970-01-08", tz = "UTC")
  expect_identical(warp_distance(x, "mday"), warp_distance(x, "day"))
})

test_that("UTC POSIXct + UTC origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")

  expect_warning(warp_distance(x, "mday"), NA)

  expect_identical(warp_distance(x, "mday"), 365)
  expect_identical(warp_distance(x, "mday", origin = x), 0)
})

test_that("UTC POSIXct + Date origin does not emit a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  origin1 <- as.Date("1971-01-01")
  origin2 <- as.Date("1972-01-01")

  expect_warning(warp_distance(x, "mday", origin = origin1), NA)

  expect_identical(warp_distance(x, "mday", origin = origin1), 0)
  expect_identical(warp_distance(x, "mday", origin = origin2), -365)
})

test_that("UTC POSIXct + non-UTC origin converts with a warning", {
  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x_with_tz <- structure(x, tzone = "America/New_York")
  origin <- as.POSIXct("1971-01-01", tz = "America/New_York")

  expect_identical(
    expect_warning(
      warp_distance(x, "mday", origin = origin),
      "`x` [(]UTC[)] and `origin` [(]America/New_York[)]"
    ),
    warp_distance(x_with_tz, "mday", origin = origin)
  )
})

test_that("local time POSIXct + UTC origin converts with a warning", {
  with_envvar(list(TZ = "America/New_York"), {
    x <- as.POSIXct("1970-12-31 23:00:00") # in UTC this is in 1971-01
    origin <- as.POSIXct("1971-01-01", tz = "UTC")

    expect_identical(
      expect_warning(warp_distance(x, "mday", origin = origin)),
      0
    )
  })
})

test_that("can use integer POSIXct", {
  x <- structure(-1L, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "mday"), -1)
})

test_that("can handle `NA` dates", {
  x <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "mday"), NA_real_)

  x <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_identical(warp_distance(x, "mday"), NA_real_)
})

test_that("can handle `every` with default origin", {
  x <- as.POSIXct(c(
    "1968-12-31", "1969-01-01",
    "1969-11-30", "1969-12-01",
    "1969-12-28", "1969-12-29",
    "1969-12-30", "1969-12-31",
    "1970-01-01",
    "1970-01-02", "1970-01-03"
  ), tz = "UTC")

  expect_equal(warp_distance(x, period = "mday", every = 2L), c(-187, -186, -17, -16, -3, -2, -2, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "mday", every = 3L), c(-128, -127, -12, -11, -2, -2, -2, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "mday", every = 4L), c(-96, -95, -9, -8, -2, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with altered origin", {
  x <- as.POSIXct(c(
    "1968-12-31", "1969-01-01",
    "1969-11-30", "1969-12-01",
    "1969-12-28", "1969-12-29",
    "1969-12-30", "1969-12-31",
    "1970-01-01",
    "1970-01-02", "1970-01-03"
  ), tz = "UTC")

  origin <- as.POSIXct("1969-12-01", "UTC")

  expect_equal(warp_distance(x, period = "mday", every = 2L, origin = origin), c(-171, -170, -1, 0, 13, 14, 14, 15, 16, 16, 17))
  expect_equal(warp_distance(x, period = "mday", every = 3L, origin = origin), c(-117, -116, -1, 0, 9, 9, 9, 10, 11, 11, 11))
  expect_equal(warp_distance(x, period = "mday", every = 4L, origin = origin), c(-88, -87, -1, 0, 6, 7, 7, 7, 8, 8, 8))
})

test_that("can handle `every` with altered origin and altered timezone", {
  x <- as.POSIXct(c(
    "1968-12-31", "1969-01-01",
    "1969-11-30", "1969-12-01",
    "1969-12-28", "1969-12-29",
    "1969-12-30", "1969-12-31",
    "1970-01-01",
    "1970-01-02", "1970-01-03"
  ), tz = "America/New_York")

  origin <- as.POSIXct("1969-12-01", tz = "America/New_York")

  expect_equal(warp_distance(x, period = "mday", every = 2L, origin = origin), c(-171, -170, -1, 0, 13, 14, 14, 15, 16, 16, 17))
  expect_equal(warp_distance(x, period = "mday", every = 3L, origin = origin), c(-117, -116, -1, 0, 9, 9, 9, 10, 11, 11, 11))
  expect_equal(warp_distance(x, period = "mday", every = 4L, origin = origin), c(-88, -87, -1, 0, 6, 7, 7, 7, 8, 8, 8))
})

test_that("going backwards in time still uses groups computed from the first of the year", {
  # The 53rd mday of 1969
  x <- as.POSIXct("1969-12-31", "UTC")
  # The 52nd mday of 1969
  y <- as.POSIXct("1969-12-30", "UTC")
  expect_identical(warp_distance(x, "mday"), -1)
  expect_identical(warp_distance(y, "mday"), -2)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "mday"),
    warp_distance(y, period = "mday")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "mday")

test_that("can warp_distance() by mday with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "mday"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "mday"), 365)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "day")

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

  expect_equal(warp_distance(x, period = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, period = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("fractional Date pieces are ignored", {
  # "1969-12-31 23:59:52 UTC"
  # .POSIXct(-0.0001 * 86400, "UTC")
  x <- structure(-0.0001, class = "Date")

  # But we really treat this as `new_date(0)`
  expect_equal(warp_distance(x, period = "day"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "day"), numeric())
  expect_equal(warp_distance(x, period = "day", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "day"), numeric())
  expect_equal(warp_distance(x, period = "day", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "day")

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

  expect_equal(warp_distance(x, period = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, period = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  expect_equal(warp_distance(x, period = "day", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-29", "1969-12-30",
    "1969-12-31", "1970-01-01",
    "1970-01-02", "1970-01-03",
    "1970-01-04"
  ), tz = "UTC")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "day", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "day", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "day", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("`as.POSIXlt()` keeps us from handling fractional negative seconds correctly on linux", {
  skip_on_os(c("linux", "solaris")) # maybe solaris? being safe

  # Base R printing is wrong, because as.POSIXlt() is wrong
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17667

  # I don't care what base R prints this as, this is supposed to be:
  # "1969-12-31T23:59:59.5"
  x <- .POSIXct(-0.5, "UTC")

  # This is
  # "1969-12-30T23:59:59.5"
  y <- .POSIXct(-86400.5, "UTC")

  # But `as.POSIXlt()` gives the wrong result by ignoring the negative
  # fractional part
  expect_failure(
    expect_identical(warp_distance(x, "day"), -1)
  )

  expect_failure(
    expect_identical(warp_distance(y, "day"), -2)
  )
})

test_that("DST is respected", {
  origin <- as.POSIXct("2018-03-11", "America/New_York")
  x <- as.POSIXct("2018-03-11 01:59:59", "America/New_York")

  # `x + 1` crosses the DST gap, so this day actually has 1 less hour.
  # We ensure that the 23:59:59 hour does not look like it has creeped into
  # the next day. If we used the pure POSIXct seconds, ignoring the time zone,
  # then we would have a problem!
  x <- x + c(0:1, 75600, 75601)

  expect_equal(warp_distance(x, "day", origin = origin), c(0, 0, 0, 1))
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "day"),
    warp_distance(y, period = "day")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "day")

test_that("can warp_distance() by day with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "day"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "day"), 365)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "hour")

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

  expect_equal(warp_distance(x, period = "hour", every = 48L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "hour", every = 72L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 96L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "hour", every = 48L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 72L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "hour", every = 96L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, period = "hour", every = 48L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "hour", every = 72L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 96L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "hour", every = 48L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 72L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "hour", every = 96L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 02:24:00 UTC"
  # structure(.1 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.1, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, period = "hour"), 0)
  expect_identical(warp_distance(y, period = "hour"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_identical(warp_distance(x, period = "hour"), numeric())
  expect_identical(warp_distance(x, period = "hour", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_identical(warp_distance(x, period = "hour"), numeric())
  expect_identical(warp_distance(x, period = "hour", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "hour")

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

  expect_equal(warp_distance(x, period = "hour", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "hour", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, period = "hour", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "hour", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  expect_equal(warp_distance(x, period = "hour", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "hour", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 21:00:00", "1969-12-31 22:00:00",
    "1969-12-31 23:00:00", "1970-01-01 00:00:00",
    "1970-01-01 01:00:00", "1970-01-01 02:00:00",
    "1970-01-01 03:00:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")

  expect_equal(warp_distance(x, period = "hour", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "hour", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "hour", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

test_that("values past microseconds are ignored", {
  x <- structure(-.000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.0000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_equal(warp_distance(x, "hour"), -1)
  expect_equal(warp_distance(y, "hour"), 0)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "hour"),
    warp_distance(y, period = "hour")
  )
})

test_that("half-hour offset time zones are correct (#13)", {
  chr <- c(
    "1970-01-01 00:00:00",
    "1970-01-01 00:50:00",
    "1970-01-01 01:00:00",
    "1970-01-01 01:20:00",
    "1970-01-01 01:30:00"
  )

  x <- as.POSIXct(chr, "Asia/Kolkata")
  y <- as.POSIXct(chr, "UTC")

  expect_equal(
    warp_distance(x, period = "hour"),
    warp_distance(y, period = "hour")
  )
})

test_that("can have an integer POSIXct origin", {
  x <- new_datetime(60 * 60, "UTC")

  origin <- structure(0L, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_identical(
    warp_distance(x, period = "hour", origin = origin),
    1
  )
})

test_that("integer Date `origin` cannot be NA_integer_", {
  x <- new_datetime(0, "UTC")
  origin <- structure(NA_integer_, class = "Date")
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")
})

test_that("double Date `origin` cannot be NA_real_ / NaN / Inf", {
  x <- new_datetime(0, "UTC")

  origin <- structure(NA_real_, class = "Date")
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")

  origin <- structure(NaN, class = "Date")
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")

  origin <- structure(Inf, class = "Date")
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")
})

test_that("double POSIXct `origin` cannot be NA_real_ / NaN / Inf", {
  x <- new_datetime(0, "UTC")

  origin <- structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")

  origin <- structure(NaN, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")

  origin <- structure(Inf, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")
})

test_that("integer POSIXct `origin` cannot be NA_integer_", {
  x <- new_datetime(0, "UTC")

  origin <- structure(NA_integer_, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  expect_error(warp_distance(x, period = "hour", origin = origin), "`origin` must be finite")
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "hour")

test_that("can warp_distance() by hour with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "hour"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "hour"), 8760)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "minute")

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

  expect_equal(warp_distance(x, period = "minute", every = 2880L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4320L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 5760L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "minute", every = 2880L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4320L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "minute", every = 5760L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, period = "minute", every = 2880L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4320L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 5760L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "minute", every = 2880L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4320L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "minute", every = 5760L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, period = "minute"), 0)
  expect_identical(warp_distance(y, period = "minute"), 0)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "minute"), numeric())
  expect_equal(warp_distance(x, period = "minute", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "minute"), numeric())
  expect_equal(warp_distance(x, period = "minute", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "minute")

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

  expect_equal(warp_distance(x, period = "minute", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "minute", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_equal(warp_distance(x, period = "minute", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "minute", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  expect_equal(warp_distance(x, period = "minute", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "minute", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:57:00", "1969-12-31 23:58:00",
    "1969-12-31 23:59:00", "1970-01-01 00:00:00",
    "1970-01-01 00:01:00", "1970-01-01 00:02:00",
    "1970-01-01 00:03:00"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:01:00", tz = "UTC")

  expect_equal(warp_distance(x, period = "minute", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "minute", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "minute", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

test_that("`origin` could have fractional components - integer POSIXct", {
  origin <- as.POSIXct("1969-12-31 23:59:59.998", "UTC")
  x <- structure(c(-120L, -60L, 0L, 58L, 59L, 60L), tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_identical(warp_distance(x, "minute", origin = origin), c(-2, -1, 0, 0, 1, 1))
})

test_that("values past microseconds are essentially ignored", {
  x <- structure(-.000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.0000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_equal(warp_distance(x, "minute"), -1)
  expect_equal(warp_distance(y, "minute"), 0)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "minute"),
    warp_distance(y, period = "minute")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "minute")

test_that("can warp_distance() by minute with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "minute"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "minute"), 525600)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "second")

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

  expect_equal(warp_distance(x, period = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, period = "second", every = 172800), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "second", every = 259200), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "second", every = 345600), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "second", every = 172800, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "second", every = 259200, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "second", every = 345600, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can ignore fractional pieces in Dates", {
  # "1970-01-01 00:07:12 UTC"
  # structure(.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  x <- structure(.005, class = "Date")

  # "1969-12-31 23:52:48 UTC"
  # structure(-.005 * 86400, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.005, class = "Date")

  expect_identical(warp_distance(x, period = "second"), 0)
  expect_identical(warp_distance(y, period = "second"), 0)
})

test_that("can handle second values larger than max int value", {
  x <- as.Date("2100-01-01")
  expect_equal(warp_distance(x, "second"), 4102444800)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "second"), numeric())
  expect_equal(warp_distance(x, period = "second", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "second"), numeric())
  expect_equal(warp_distance(x, period = "second", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "second")

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

  expect_identical(warp_distance(x, period = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, period = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, period = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
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

  expect_identical(warp_distance(x, period = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_identical(warp_distance(x, period = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_identical(warp_distance(x, period = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  expect_identical(warp_distance(x, period = "second", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, period = "second", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, period = "second", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:57", "1969-12-31 23:59:58",
    "1969-12-31 23:59:59", "1970-01-01 00:00:00",
    "1970-01-01 00:00:01", "1970-01-01 00:00:02",
    "1970-01-01 00:00:03"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:00:01", tz = "UTC")

  expect_equal(warp_distance(x, period = "second", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "second", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "second", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

test_that("values past microseconds are essentially ignored", {
  x <- structure(-.000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.0000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_equal(warp_distance(x, "second"), -1)
  expect_equal(warp_distance(y, "second"), 0)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "second"),
    warp_distance(y, period = "second")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "second")

test_that("can warp_distance() by second with POSIXlt", {
  x <- as.POSIXct("1970-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "second"), 0)

  x <- as.POSIXct("1971-01-01", tz = "UTC")
  x <- as.POSIXlt(x)
  expect_identical(warp_distance(x, "second"), 31536000)
})

# ------------------------------------------------------------------------------
# warp_distance(<Date>, period = "millisecond")

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

  expect_equal(warp_distance(x, period = "millisecond", every = 172800 * 1000), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 259200 * 1000), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 345600 * 1000), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - integer Dates", {
  x <- structure(-3L:3L, class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "millisecond", every = 172800 * 1000, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 259200 * 1000, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "millisecond", every = 345600 * 1000, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle `every` with default origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  expect_equal(warp_distance(x, period = "millisecond", every = 172800 * 1000), c(-2, -1, -1, 0, 0, 1, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 259200 * 1000), c(-1, -1, -1, 0, 0, 0, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 345600 * 1000), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric Dates", {
  x <- structure(as.numeric(-3:3), class = "Date")

  origin <- as.Date("1970-01-02")

  expect_equal(warp_distance(x, period = "millisecond", every = 172800 * 1000, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 259200 * 1000, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "millisecond", every = 345600 * 1000, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
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

  expect_identical(warp_distance(x, period = "millisecond"), 0)
  expect_identical(warp_distance(y, period = "millisecond"), 0)
  expect_identical(warp_distance(z, period = "millisecond"), 86400000)
})

test_that("can handle millisecond values larger than max int value", {
  x <- as.Date("2100-01-01")
  expect_equal(warp_distance(x, "millisecond"), 4102444800 * 1000)
})

test_that("size 0 input works - integer Dates", {
  x <- structure(integer(), class = "Date")

  expect_equal(warp_distance(x, period = "millisecond"), numeric())
  expect_equal(warp_distance(x, period = "millisecond", every = 2), numeric())
})

test_that("size 0 input works - numeric Dates", {
  x <- structure(numeric(), class = "Date")

  expect_equal(warp_distance(x, period = "millisecond"), numeric())
  expect_equal(warp_distance(x, period = "millisecond", every = 2), numeric())
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXct>, period = "millisecond")

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

  expect_identical(warp_distance(x, period = "millisecond", every = 2L), c(-2, -1, -1, 0, 0, 1, 1))
  expect_identical(warp_distance(x, period = "millisecond", every = 3L), c(-1, -1, -1, 0, 0, 0, 1))
  expect_identical(warp_distance(x, period = "millisecond", every = 4L), c(-1, -1, -1, 0, 0, 0, 0))
})

test_that("can handle `every` with altered origin - numeric POSIXct", {
  x <- as.POSIXct(c(
    "1969-12-31 23:59:59.997", "1969-12-31 23:59:59.998",
    "1969-12-31 23:59:59.999", "1970-01-01 00:00:00.000",
    "1970-01-01 00:00:00.001", "1970-01-01 00:00:00.002",
    "1970-01-01 00:00:00.003"
  ), tz = "UTC")

  origin <- as.POSIXct("1970-01-01 00:00:00.001", tz = "UTC")

  expect_equal(warp_distance(x, period = "millisecond", every = 2L, origin = origin), c(-2, -2, -1, -1, 0, 0, 1))
  expect_equal(warp_distance(x, period = "millisecond", every = 3L, origin = origin), c(-2, -1, -1, -1, 0, 0, 0))
  expect_equal(warp_distance(x, period = "millisecond", every = 4L, origin = origin), c(-1, -1, -1, -1, 0, 0, 0))
})

test_that("can handle fractional pieces with decimilliseconds correctly", {
  x <- as.POSIXct("1969-12-31 23:59:59.9989", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -2)

  x <- as.POSIXct("1969-12-31 23:59:59.9995", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -1)

  x <- as.POSIXct("1969-12-31 23:59:59.9999", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00.0000", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.0009", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.0010", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 1)

  x <- as.POSIXct("1970-01-01 00:00:00.0011", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 1)
})

test_that("can handle fractional pieces with centimilliseconds correctly", {
  # Base R is going to print these wrong, use {nanotime} if you need to
  # print them correctly
  x <- as.POSIXct("1969-12-31 23:59:59.99809", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -2)

  x <- as.POSIXct("1969-12-31 23:59:59.99905", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -1)

  x <- as.POSIXct("1969-12-31 23:59:59.99909", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), -1)

  x <- as.POSIXct("1970-01-01 00:00:00.00000", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.00009", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 0)

  x <- as.POSIXct("1970-01-01 00:00:00.00100", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 1)

  x <- as.POSIXct("1970-01-01 00:00:00.00101", "UTC")
  expect_identical(warp_distance(x, period = "millisecond"), 1)
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

  expect_identical(warp_distance(x, period = "millisecond", every = 1L, origin = origin), c(-1, 0, 1, 2, 3, 4, 5))
  expect_identical(warp_distance(x, period = "millisecond", every = 2L, origin = origin), c(-1, 0, 0, 1, 1, 2, 2))
})

test_that("`origin` could have floating point error values that need guarding - integer POSIXct", {
  # "1970-01-01 00:00:00 UTC" "1969-12-31 23:59:59 UTC"
  x <- structure(c(0L, -1L), tzone = "UTC", class = c("POSIXct", "POSIXt"))

  # We know this one can't be represented exactly in floating point
  origin <- as.POSIXct("1969-12-31 23:59:59.998", "UTC")

  expect_identical(warp_distance(x, period = "millisecond", every = 1L, origin = origin), c(2, -998))
})

test_that("values past microseconds are essentially ignored", {
  x <- structure(-.000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.0000002, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_equal(warp_distance(x, "millisecond"), -1)
  expect_equal(warp_distance(y, "millisecond"), 0)

  x <- structure(-.001002, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  y <- structure(-.0010002, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_equal(warp_distance(x, "millisecond"), -2)
  expect_equal(warp_distance(y, "millisecond"), -1)
})

test_that("proof that the +1e-7 guard is necessary", {
  # Found this through trial and error
  # I was looking for a place where the scaling up then down by 1e6
  # produces a value that is represented as being `< x`
  # Here we get:

  # 1.000000001327000021935e+09 (start)
  # *1e6
  # 1.000000001327000000000e+15
  # trunc
  # 1.000000001327000000000e+15
  # *1e-6
  # 1.000000001326999902725e+09 (here is the problem! guard required!)
  # +1e-7
  # 1.000000001327000021935e+09 (no guard: 1.000000001326999902725e+09)
  # *1e3
  # 1.000000001327000000000e+12 (no guard: 1.000000001326999877930e+12)
  # floor
  # 1.000000001327000000000e+12 (no guard: 1.000000001326000000000e+12)

  x <- structure(1000000001.327, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  expect_identical(warp_distance(x, "millisecond"), 1000000001327)
})

test_that("default `origin` results in epoch in the time zone of `x`", {
  x <- as.POSIXct("1969-12-31 23:00:00", tz = "America/New_York")
  y <- as.POSIXct("1969-12-31 23:00:00", tz = "UTC")

  expect_equal(
    warp_distance(x, period = "millisecond"),
    warp_distance(y, period = "millisecond")
  )
})

# ------------------------------------------------------------------------------
# warp_distance(<POSIXlt>, period = "millisecond")

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
  expect_error(warp_distance(1, period = "year"), "must inherit from")
})

test_that("`origin` is validated", {
  expect_error(warp_distance(new_date(0), period = "year", origin = 1), "must inherit from")
  expect_error(warp_distance(new_date(0), period = "year", origin = new_date(c(0, 1))), "size 1, not 2")

  expect_error(warp_distance(new_date(0), period = "year", origin = new_date(NA_real_)), "cannot be `NA`")
  expect_error(warp_distance(new_date(0), period = "month", origin = new_date(NA_real_)), "cannot be `NA`")
})

test_that("`every` is validated", {
  expect_error(warp_distance(new_date(0), period = "year", every = 0), "greater than 0, not 0")
  expect_error(warp_distance(new_date(0), period = "year", every = -1), "greater than 0, not -1")
  expect_error(warp_distance(new_date(0), period = "year", every = structure(1, class = "foobar")), "bare integer-ish")
  expect_error(warp_distance(new_date(0), period = "year", every = "x"), "integer-ish, not character")
  expect_error(warp_distance(new_date(0), period = "year", every = c(1, 1)), "size 1, not 2")
  expect_error(warp_distance(new_date(0), period = "year", every = integer()), "size 1, not 0")
  expect_error(warp_distance(new_date(0), period = "year", every = NA_integer_), "`every` must not be `NA`")
})

test_that("`period` is validated", {
  expect_error(warp_distance(new_date(0), period = 1), "single string")
  expect_error(warp_distance(new_date(0), period = c("x", "y")), "single string")
  expect_error(warp_distance(new_date(0), period = "yr"), "Unknown `period` value 'yr'")
})

test_that("optional arguments must be specified by name", {
  expect_error(
    warp_distance(new_date(0), "year", 1),
    "`...` is not empty in `warp_distance[(][)]`."
  )
})
