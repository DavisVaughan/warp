#' Locate change points in a vector
#'
#' `locate_changes()` determines where an integer or double vector changes
#' values. It returns an integer vector containing the positions of the last
#' element before the change. These are the "stop points" used in
#' [locate_boundaries()] and [warp_boundaries()]. The position of the last
#' element of `x` is always returned at the end of the vector.
#'
#' @param x `[integer / double]`
#'
#'   The vector to locate changes in. Can only be an integer or double vector.
#'
#' @return
#'
#'   An integer vector mapping to the last value of `x` before a change is
#'   detected.
#'
#' @export
#' @examples
#' locate_changes(c(1, 1, 2))
#'
#' locate_changes(c(1.5, 1, 2))
#'
#' # NA and NaN values are treated as being equivalent with themselves
#' # but not with each other
#' locate_changes(c(1, NA_real_, NA_real_, NaN, NaN))
locate_changes <- function(x) {
  .Call(timewarp_locate_changes, x)
}
