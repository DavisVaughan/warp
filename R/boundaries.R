# `x` = Date / POSIXct
# Converts to ranges of starts/stops breaking it up into period groups
# Calls `warp_group()` then `locate_boundaries()`

warp_boundaries <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timewarp_warp_boundaries, x, by, every, origin)
}
