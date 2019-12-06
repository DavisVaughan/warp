# This does everything `locate_changes()` does, but then creates a two column
# data frame of `starts` and `stops` that can be used to slice `x` into its
# groups
# The `stops` are the exact result of `locate_changes()`
# The `starts` are computed from the `stops`

locate_boundaries <- function(x) {
  .Call(timewarp_locate_boundaries, x)
}
