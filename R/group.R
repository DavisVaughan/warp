warp_group <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timeslide_warp_group, x, by, every, origin)
}
