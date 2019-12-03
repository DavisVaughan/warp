uniquify <- function(x, by = "year") {
  by <- vec_assert(by, ptype = character(), size = 1L)
  by <- normalize_period(by)

  .Call(timeslide_uniquify, x, by)
}

normalize_period <- function(x) {
  switch(
    x,

    year = ,
    years =,
    yearly = 1L,

    month =,
    months =,
    monthly = 2L,

    day = ,
    days = ,
    daily = 4L,

    stop("Unknown period specification.", call. = FALSE)
  )
}
