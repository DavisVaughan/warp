dots_n <- function(...) {
  nargs()
}

# Like `ellipsis::check_dots_empty()` but without the import
check_dots_empty <- function(fn, ...) {
  n <- dots_n(...)

  if (n == 0L) {
    return(invisible())
  }

  msg <- paste0(
    "`...` is not empty in `", fn, "()`.\n",
    "These dots only exist to allow for future extensions and should be empty.\n",
    "Did you misspecify an argument?"
  )

  stop(msg, call. = FALSE)
}
