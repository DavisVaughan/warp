new_date <- function(x = double()) {
  stopifnot(is.double(x))

  structure(
    x,
    class = "Date"
  )
}

new_datetime <- function(x = double(), tzone = "") {
  if (is.null(tzone)) {
    tzone <- ""
  }

  if (is.integer(x)) {
    x <- as.double(x)
  }
  stopifnot(is.double(x))
  stopifnot(is.character(tzone))

  structure(
    x,
    tzone = tzone,
    class = c("POSIXct", "POSIXt")
  )
}
