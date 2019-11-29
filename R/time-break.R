time_break <- function(x, by = "year") {
  x <- uniquify(x, by)
  breakpoints(x)
}
