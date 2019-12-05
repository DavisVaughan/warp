# `x` = Date / POSIXct
# Converts to ranges of starts/stops breaking it up into period groups
# Calls `warp_group()` then `warp_ranges()`

warp_breaks <- function(x, by = "year", every = 1L, origin = NULL) {
  .Call(timewarp_warp_breaks, x, by, every, origin)
}
