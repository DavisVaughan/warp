# Take an integer/double vector and locate the positions where a value changes
# Returns an integer vector of the position right before a change
# Always returns the position of the last value in the vector

warp_changes <- function(x) {
  .Call(timewarp_warp_changes, x)
}
