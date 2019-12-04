#' @export
time_break <- function(x, by = "year") {
  x <- warp_group(x, by)
  breakpoints(x)
}
