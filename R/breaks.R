warp_breaks <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timewarp_warp_breaks, x, by, every, origin)
}
