# Exported for testing

date_get_year_offset <- function(x) {
  .Call(warp_date_get_year_offset, x)
}

date_get_month_offset <- function(x) {
  .Call(warp_date_get_month_offset, x)
}

# nocov start

divmod <- function(x, y) {
  .Call(warp_divmod, x, y)
}

div <- function(x, y) {
  .Call(warp_div, x, y)
}

# nocov end
