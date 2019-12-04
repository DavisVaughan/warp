time_class_type <- function(x) {
  .Call(timeslide_class_type, x)
}

# Callable from C
as_posixct_from_posixlt <- function(x) {
  as.POSIXct.POSIXlt(x)
}
