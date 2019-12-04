warp_changes <- function(x) {
  .Call(timewarp_warp_changes, x)
}

warp_ranges <- function(x) {
  .Call(timewarp_warp_ranges, x)
}
