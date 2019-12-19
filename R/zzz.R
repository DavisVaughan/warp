.onLoad <- function(libname, pkgname) {
  .Call(warp_init_library, asNamespace("warp"))
}
