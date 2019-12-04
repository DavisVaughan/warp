warp_chunk <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timeslide_warp_chunk, x, by, every, origin)
}
