#include "warp.h"
#include "utils.h"

// - We need the year to be the number of years since 1970, so we need to
//   subtract 70 from the result we get from as.POSIXlt() (it gives it to us
//   from 1900).
// - For months, 0-11 is useful as the range.

// -----------------------------------------------------------------------------

static SEXP posixct_get_year(SEXP x);
static SEXP posixlt_get_year(SEXP x);

// [[ "utils.h" ]]
SEXP get_year(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_year(x);
  case warp_class_posixct: return posixct_get_year(x);
  case warp_class_posixlt: return posixlt_get_year(x);
  default: r_error("get_year", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_year(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_year(x);
  UNPROTECT(1);
  return out;
}

static SEXP posixlt_get_year(SEXP x) {
  SEXP out = VECTOR_ELT(x, 5);
  out = PROTECT(r_maybe_duplicate(out));

  if (TYPEOF(out) != INTSXP) {
    r_error(
      "posixlt_get_year",
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

static SEXP posixct_get_year_month(SEXP x);
static SEXP posixlt_get_year_month(SEXP x);

// [[ "utils.h" ]]
SEXP get_year_month(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_year_month(x);
  case warp_class_posixct: return posixct_get_year_month(x);
  case warp_class_posixlt: return posixlt_get_year_month(x);
  default: r_error("get_year", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_year_month(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_year_month(x);
  UNPROTECT(1);
  return out;
}

static SEXP posixlt_get_year_month(SEXP x) {
  SEXP year = VECTOR_ELT(x, 5);
  year = PROTECT(r_maybe_duplicate(year));

  SEXP month = VECTOR_ELT(x, 4);

  if (TYPEOF(year) != INTSXP) {
    r_error(
      "posixlt_get_year_month",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(month) != INTSXP) {
    r_error(
      "posixlt_get_year_month",
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
