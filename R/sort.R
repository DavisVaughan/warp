warp_is_sorted <- function(x, strictly = TRUE, increasing = TRUE, na_value = "largest") {
  .Call(warp_warp_is_sorted, x, strictly, increasing, na_value)
}
