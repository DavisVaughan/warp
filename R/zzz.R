.onLoad <- function(libname, pkgname) {
  # Load timerip namespace for access to C callables
  requireNamespace("timerip", quietly = TRUE)

  # Initialize timerip API
  .Call(timeslide_init_timerip)
}
