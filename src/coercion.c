#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// `as_datetime()` ensures that Dates are converted to POSIXct without changing
// the clock time. A UTC time zone is always attached. POSIXlt is converted to
// POSIXct by calling `as.POSIXct()` on the R side through
// `as_posixct_from_posixlt()`.

static SEXP as_datetime_from_date(SEXP x);
static SEXP as_datetime_from_posixct(SEXP x);
static SEXP as_datetime_from_posixlt(SEXP x);

// [[ include("utils.h") ]]
SEXP as_datetime(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return as_datetime_from_date(x);
  case warp_class_posixct: return as_datetime_from_posixct(x);
  case warp_class_posixlt: return as_datetime_from_posixlt(x);
  case warp_class_unknown: r_error("as_datetime", "Internal error: Unknown date time class.");
  }
  never_reached("as_datetime");
}

#define AS_DATETIME_FROM_DATE_LOOP(CTYPE, CONST_DEREF, NA_CHECK) {   \
  const CTYPE* p_x = CONST_DEREF(x);                                 \
                                                                     \
  for (R_xlen_t i = 0; i < x_size; ++i) {                            \
    const CTYPE elt = p_x[i];                                        \
                                                                     \
    if (NA_CHECK) {                                                  \
      p_out[i] = NA_REAL;                                            \
      continue;                                                      \
    }                                                                \
                                                                     \
    p_out[i] = p_x[i] * 86400;                                       \
  }                                                                  \
}

static SEXP as_datetime_from_date(SEXP x) {
  R_xlen_t x_size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  switch (TYPEOF(x)) {
  case INTSXP: AS_DATETIME_FROM_DATE_LOOP(int, INTEGER_RO, elt == NA_INTEGER); break;
  case REALSXP: AS_DATETIME_FROM_DATE_LOOP(double, REAL_RO, !R_FINITE(elt)); break;
  default: Rf_errorcall(R_NilValue, "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }

  SEXP strings_utc = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(strings_utc, 0, Rf_mkChar("UTC"));

  Rf_setAttrib(out, Rf_install("tzone"), strings_utc);

  SEXP classes_posixct = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes_posixct, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(classes_posixct, 1, Rf_mkChar("POSIXt"));

  Rf_setAttrib(out, R_ClassSymbol, classes_posixct);

  UNPROTECT(3);
  return out;
}

#undef AS_DATETIME_FROM_DATE_LOOP

// Convert integer POSIXct (if they ever happen) to double
static SEXP as_datetime_from_posixct(SEXP x) {
  SEXPTYPE type = TYPEOF(x);

  if (type == REALSXP) {
    return x;
  }

  if (type != INTSXP) {
    Rf_errorcall(R_NilValue, "A 'POSIXct' can only be an integer or double.");
  }

  R_xlen_t x_size = Rf_xlength(x);

  const int* p_x = INTEGER_RO(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
    } else {
      p_out[i] = (double) elt;
    }
  }

  SET_ATTRIB(out, ATTRIB(x));

  UNPROTECT(1);
  return out;
}

static SEXP as_datetime_from_posixlt(SEXP x) {
  return as_posixct_from_posixlt(x);
}
