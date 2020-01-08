#include "warp.h"
#include "utils.h"
#include "divmod.h"

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

// -----------------------------------------------------------------------------

static inline int days_before_year(int year_offset);
static inline int days_before_month(int year, int month);
static inline SEXP make_tzone(const char* time_zone);

// [[ include("utils.h") ]]
SEXP as_start_of_day_posixct_from_posixlt(SEXP x) {
  SEXP year_offset = VECTOR_ELT(x, 5);
  SEXP month = VECTOR_ELT(x, 4);
  SEXP day = VECTOR_ELT(x, 3);

  int* p_year_offset = INTEGER(year_offset);
  int* p_month = INTEGER(month);
  int* p_day = INTEGER(day);

  R_xlen_t size = Rf_xlength(year_offset);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt_year_offset = p_year_offset[i];

    if (elt_year_offset == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    int elt_year = elt_year_offset + 1900;
    int elt_month = p_month[i];
    int elt_day = p_day[i] - 1;

    int days =
      days_before_year(elt_year_offset) +
      days_before_month(elt_year, elt_month) +
      elt_day;

    p_out[i] = days * 86400;
  }

  Rf_setAttrib(out, syms_tzone, strings_utc);
  Rf_setAttrib(out, syms_class, classes_posixct);

  const char* time_zone = get_time_zone(x);
  out = PROTECT(force_tz(out, make_tzone(time_zone)));

  UNPROTECT(2);
  return out;
}

#define is_leap_year(year) ((((year) % 4) == 0 && ((year) % 100) != 0) || ((year) % 400) == 0)

static const int DAYS_BEFORE_MONTH[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

// `year` is the full year
// `month` is 0-based
static inline int days_before_month(int year, int month) {
  return DAYS_BEFORE_MONTH[month] + (month > 1 && is_leap_year(year));
}

#undef is_leap_year


#define YEARS_FROM_0001_01_01_TO_1900 1899
#define DAYS_FROM_0001_01_01_TO_EPOCH 719162

static inline int days_before_year(int year_offset) {
  int year = year_offset + YEARS_FROM_0001_01_01_TO_1900;

  int days = year * 365 +
    int_div(year, 4) -
    int_div(year, 100) +
    int_div(year, 400);

  days -= DAYS_FROM_0001_01_01_TO_EPOCH;

  return days;
}

#undef YEARS_FROM_0001_01_01_TO_1900
#undef DAYS_FROM_0001_01_01_TO_EPOCH

static inline SEXP make_tzone(const char* time_zone) {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));

  SET_STRING_ELT(out, 0, Rf_mkChar(time_zone));

  UNPROTECT(1);
  return out;
}
