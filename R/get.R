# Called from C

time_get <- function(x, components = NULL) {
  # Hard code for now
  week_start <- 1L

  validate_supported_date_time_class(x)
  validate_components(components)
  validate_week_start(week_start)

  map_allowed_to_out_pos <- map_allowed_names_to_out_pos()
  map_out_pos_to_posixlt <- map_out_pos_to_posixlt_names()

  if (is.null(components)) {
    out_pos <- map_allowed_to_out_pos
  } else {
    out_pos <- map_allowed_to_out_pos[components]
  }

  n_x <- length(x)

  if (length(out_pos) == 0L) {
    return(time_get_template_no_columns(n_x))
  }

  # Maintain ordering, and remove duplicates
  out_pos <- unique(out_pos)
  out_pos <- sort(out_pos)

  if (n_x == 0L) {
    return(time_get_template_no_rows()[out_pos])
  }

  names_lt <- map_out_pos_to_posixlt[out_pos]

  out <- as_posixlt(x)
  out <- unclass(out)[names_lt]
  names(out) <- names(names_lt)

  if (!is.null(out$year)) {
    out$year <- out$year + 1900L
  }

  if (!is.null(out$yday)) {
    out$yday <- out$yday + 1L
  }

  if (!is.null(out$month)) {
    out$month <- out$month + 1L
  }

  if (!is.null(out$wday)) {
    out$wday <- 1L + (out$wday + (6L - week_start)) %% 7L
  }

  as.data.frame(out)
}

as_posixlt <- function(x) {
  type <- time_class_type(x)

  if (type == "posixlt") {
    return(x)
  }

  # `as.POSIXlt.Date()` is SO slow
  if (type == "date") {
    origin <- structure(0, class = "Date")
    out <- as.POSIXlt(unclass(x) * 86400, tz = "UTC", origin = origin)
    return(out)
  }

  if (type == "posixct") {
    out <- as.POSIXlt.POSIXct(x)
    return(out)
  }

  stop("`x` has an unknown date time class", call. = FALSE)
}

time_get_template_no_columns <- function(n) {
  structure(
    list(),
    names = character(),
    class = "data.frame",
    row.names = .set_row_names(n)
  )
}

time_get_template_no_rows <- function() {
  data.frame(
    year = integer(0),
    month = integer(0),
    yday = integer(0),
    mday = integer(0),
    wday = integer(0),
    hour = integer(0),
    minute = integer(0),
    second = numeric(0)
  )
}

allowed_components <- function() {
  c(
    "year",
    "month",
    "yday",
    "mday",
    "day",
    "wday",
    "hour",
    "minute",
    "second"
  )
}

map_allowed_names_to_out_pos <- function() {
  c(
    year = 1L,
    month = 2L,
    yday = 3L,
    mday = 4L,
    day = 4L,
    wday = 5L,
    hour = 6L,
    minute = 7L,
    second = 8L
  )
}

map_out_pos_to_posixlt_names <- function() {
  c(
    year = "year",
    month = "mon",
    yday = "yday",
    mday = "mday",
    wday = "wday",
    hour = "hour",
    minute = "min",
    second = "sec"
  )
}

validate_week_start <- function(week_start) {
  if (length(week_start) != 1L) {
    stop("`week_start` must be a single integer.", call. = FALSE)
  }

  valid_week_start <- week_start %in% 1:7
  if (!valid_week_start) {
    stop("`week_start` must be in the range of `1:7`.", call. = FALSE)
  }

  invisible(week_start)
}

validate_components <- function(components) {
  if (is.null(components)) {
    return(invisible(components))
  }

  if (!is.character(components)) {
    stop("`components` must be a character vector of components, or `NULL`.", call. = FALSE)
  }

  is_allowed <- components %in% allowed_components()

  if (!all(is_allowed)) {
    components <- components[!is_allowed]
    components <- sQuote(components, q = FALSE)
    components <- paste0(components, collapse = ", ")

    stop("`components` must be an allowed component, not: ", components, call. = FALSE)
  }

  invisible(components)
}

validate_supported_date_time_class <- function(x, arg = "`x`") {
  if (!is_supported_date_time_class(x)) {
    class <- paste0(class(x), collapse = "/")
    stop(arg, " must be a supported date time class, not: ", class, call. = FALSE)
  }
  invisible(x)
}

is_supported_date_time_class <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXlt"))
}
