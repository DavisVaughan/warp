# This does everything `warp_changes()` does, but then creates a two column
# data frame of `starts` and `stops` that can be used to slice `x` into its
# groups
# The `stops` are the exact result of `warp_changes()`
# The `starts` are computed from the `stops`

warp_ranges <- function(x) {
  .Call(timewarp_warp_ranges, x)
}
