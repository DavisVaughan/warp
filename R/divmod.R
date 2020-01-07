# Exported for testing

divmod <- function(x, y) {
  .Call(warp_divmod, x, y)
}

div <- function(x, y) {
  .Call(warp_div, x, y)
}
