# Exported for testing

date_get_year <- function(x) {
  .Call(warp_date_get_year, x)
}

date_get_year_month <- function(x) {
  .Call(warp_date_get_year_month, x)
}

divmod <- function(x, y) {
  .Call(warp_divmod, x, y)
}

div <- function(x, y) {
  .Call(warp_div, x, y)
}
