#include "warp.h"
#include "utils.h"

/*
 * `get_year_offset()`
 *   Extract the number of years offset from 1970.
 *   Returns an integer vector.
 *
 * `get_year_month_offset()`
 *   Extract the number of years offset from 1970.
 *   Extract the month offset as an integer in the range of 0-11.
 *   Return a list of both.
 */

// -----------------------------------------------------------------------------

static SEXP posixct_get_year_offset(SEXP x);
static SEXP posixlt_get_year_offset(SEXP x);

// [[ "utils.h" ]]
SEXP get_year_offset(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_year_offset(x);
  case warp_class_posixct: return posixct_get_year_offset(x);
  case warp_class_posixlt: return posixlt_get_year_offset(x);
  default: r_error("get_year_offset", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_year_offset(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_year_offset(x);
  UNPROTECT(1);
  return out;
}

static SEXP posixlt_get_year_offset(SEXP x) {
  SEXP out = VECTOR_ELT(x, 5);
  out = PROTECT(r_maybe_duplicate(out));

  if (TYPEOF(out) != INTSXP) {
    r_error(
      "posixlt_get_year_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  int* p_out = INTEGER(out);

  R_xlen_t n = Rf_xlength(out);

  for (R_xlen_t i = 0; i < n; ++i) {
    if (p_out[i] == NA_INTEGER) {
      continue;
    }

    p_out[i] -= 70;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP posixct_get_year_month_offset(SEXP x);
static SEXP posixlt_get_year_month_offset(SEXP x);

// [[ "utils.h" ]]
SEXP get_year_month_offset(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_year_month_offset(x);
  case warp_class_posixct: return posixct_get_year_month_offset(x);
  case warp_class_posixlt: return posixlt_get_year_month_offset(x);
  default: r_error("get_year_offset", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_year_month_offset(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_year_month_offset(x);
  UNPROTECT(1);
  return out;
}

static SEXP posixlt_get_year_month_offset(SEXP x) {
  SEXP year = VECTOR_ELT(x, 5);
  year = PROTECT(r_maybe_duplicate(year));

  SEXP month = VECTOR_ELT(x, 4);

  if (TYPEOF(year) != INTSXP) {
    r_error(
      "posixlt_get_year_month_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(month) != INTSXP) {
    r_error(
      "posixlt_get_year_month_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  int* p_year = INTEGER(year);

  R_xlen_t n = Rf_xlength(year);

  for (R_xlen_t i = 0; i < n; ++i) {
    if (p_year[i] == NA_INTEGER) {
      continue;
    }

    p_year[i] -= 70;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(2);
  return out;
}
