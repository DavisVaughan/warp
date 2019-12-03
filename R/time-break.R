#' @export
time_break <- function(x, by = "year") {
  x <- warp_chunk(x, by)
  breakpoints(x)
}
