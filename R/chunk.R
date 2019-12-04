warp_chunk <- function(x, by = "year", origin = NULL) {
  .Call(timeslide_warp_chunk, x, by, origin)
}
