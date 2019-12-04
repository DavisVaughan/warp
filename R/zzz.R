.onLoad <- function(libname, pkgname) {
  .Call(timewarp_init_library, asNamespace("timewarp"))
}
