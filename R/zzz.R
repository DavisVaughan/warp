.onLoad <- function(libname, pkgname) {
  .Call(timeslide_init_library, asNamespace("timeslide"))
}
