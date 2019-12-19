time_class_type <- function(x) {
  .Call(warp_class_type, x)
}

single_quote <- function(x) {
  encodeString(x, quote = "'", na.encode = FALSE)
}

# Callable from C
as_posixct_from_posixlt <- function(x) {
  as.POSIXct.POSIXlt(x)
}

# Callable from C, ensures that the resulting Date
# is a double and has no fractional parts
as_date <- function(x) {
  type <- time_class_type(x)

  if (type == "date") {
    if (typeof(x) == "integer") {
      return(structure(as.double(x), class = "Date"))
    } else {
      # Always truncate towards 0 to get rid of fractional date components
      return(structure(trunc(unclass(x)), class = "Date"))
    }
  }

  if (type == "posixct") {
    return(as.Date.POSIXct(x, tz = tz(x)))
  }

  if (type == "posixlt") {
    return(as.Date.POSIXlt(x))
  }

  stop("Internal error: Unknown date time class", call. = FALSE)
}

# Used in `as_date()`, main thing to ensure of is that
# `as_date()` on a POSIXct retains the year/month/day of that time zone
tz <- function(x) {
  tzone <- attr(x, "tzone")[[1]]

  if (is.null(tzone) && !is_POSIXt(x)) {
    return("UTC")
  }

  if (is.character(tzone) && nzchar(tzone)) {
    return(tzone)
  }

  tzone <- attr(as.POSIXlt(x[0]), "tzone")[[1]]

  if (is.null(tzone)) {
    return("UTC")
  }

  tzone
}

is_POSIXt <- function(x) {
  inherits(x, "POSIXt")
}

# Called from C
as_posixlt <- function(x) {
  type <- time_class_type(x)

  if (type == "posixlt") {
    return(x)
  }

  # `as.POSIXlt.Date()` is SO slow
  if (type == "date") {
    origin <- structure(0, class = "Date")
    x <- unclass(x)

    # Ignore fractional Date pieces by truncating towards 0
    if (typeof(x) == "double") {
      x <- trunc(x)
    }

    out <- as.POSIXlt(x * 86400, tz = "UTC", origin = origin)
    return(out)
  }

  if (type == "posixct") {
    out <- as.POSIXlt.POSIXct(x)
    return(out)
  }

  stop("`x` has an unknown date time class", call. = FALSE)
}

