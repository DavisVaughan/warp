# Exported for testing

date_get_year <- function(x) {
  .Call(warp_date_get_year, x)
}

date_get_year_month <- function(x) {
  .Call(warp_date_get_year_month, x)
}
