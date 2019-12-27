#include "warp.h"
#include "utils.h"
#include "divmod.h"

/*
 * `get_year_offset()`
 *   Extract the number of years offset from 1970.
 *   Returns an integer vector.
 *
 * `get_month_offset()`
 *   Extract the number of months offset from 1970.
 *   Return an integer vecctor.
 *
 * `get_day_offset()`
 *   Extract the number of days offset from 1970.
 *   Return an integer vector.
 *
 * `get_week_offset()`
 *   Extract the number of weeks offset from 1970, with a restart of the 7
 *   day counter every January 1st.
 *   Return an integer vector
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

static SEXP posixct_get_month_offset(SEXP x);
static SEXP posixlt_get_month_offset(SEXP x);

// [[ "utils.h" ]]
SEXP get_month_offset(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_month_offset(x);
  case warp_class_posixct: return posixct_get_month_offset(x);
  case warp_class_posixlt: return posixlt_get_month_offset(x);
  default: r_error("get_month_offset", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_month_offset(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_month_offset(x);
  UNPROTECT(1);
  return out;
}

#define YEARS_FROM_1900_TO_1970 70
#define MONTHS_IN_YEAR 12

static SEXP posixlt_get_month_offset(SEXP x) {
  SEXP year = VECTOR_ELT(x, 5);
  SEXP month = VECTOR_ELT(x, 4);

  if (TYPEOF(year) != INTSXP) {
    r_error(
      "posixlt_get_month_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(month) != INTSXP) {
    r_error(
      "posixlt_get_month_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  int* p_year = INTEGER(year);
  int* p_month = INTEGER(month);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_year[i] == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = (p_year[i] - YEARS_FROM_1900_TO_1970) * MONTHS_IN_YEAR + p_month[i];
  }

  UNPROTECT(1);
  return out;
}

#undef YEARS_FROM_1900_TO_1970
#undef MONTHS_IN_YEAR

// -----------------------------------------------------------------------------

static SEXP date_get_day_offset(SEXP x);
static SEXP posixct_get_day_offset(SEXP x);
static SEXP posixlt_get_day_offset(SEXP x);

// [[ "utils.h" ]]
SEXP get_day_offset(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_day_offset(x);
  case warp_class_posixct: return posixct_get_day_offset(x);
  case warp_class_posixlt: return posixlt_get_day_offset(x);
  default: r_error("get_day_offset", "Internal error: Unknown date time class.");
  }
}

static SEXP int_date_get_day_offset(SEXP x);
static SEXP dbl_date_get_day_offset(SEXP x);

static SEXP date_get_day_offset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_day_offset(x);
  case REALSXP: return dbl_date_get_day_offset(x);
  default: r_error("date_get_day_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

static SEXP int_date_get_day_offset(SEXP x) {
  return x;
}

static SEXP dbl_date_get_day_offset(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Truncate any fractional pieces towards 0
  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_x[i] == NA_REAL) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = (int) p_x[i];
  }

  UNPROTECT(1);
  return out;
}

static SEXP posixct_get_day_offset(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_day_offset(x);
  UNPROTECT(1);
  return out;
}

static inline int days_before_year(int year);

static SEXP posixlt_get_day_offset(SEXP x) {
  SEXP year = VECTOR_ELT(x, 5);
  SEXP yday = VECTOR_ELT(x, 7);

  if (TYPEOF(year) != INTSXP) {
    r_error(
      "posixlt_get_day_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(yday) != INTSXP) {
    r_error(
      "posixlt_get_day_offset",
      "Internal error: The 8th element of the POSIXlt object should be an integer."
    );
  }

  int* p_year = INTEGER(year);
  int* p_yday = INTEGER(yday);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_year[i] == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = days_before_year(p_year[i]) + p_yday[i];
  }

  UNPROTECT(1);
  return out;
}

// Not 1899 because `year` is 0 based already
#define YEARS_FROM_0001_01_01_TO_1900 1900
#define DAYS_FROM_0001_01_01_TO_EPOCH 719162

static inline int days_before_year(int year) {
  year += YEARS_FROM_0001_01_01_TO_1900;
  year -= 1;

  int days = year * 365 +
    int_div(year, 4) -
    int_div(year, 100) +
    int_div(year, 400);

  days -= DAYS_FROM_0001_01_01_TO_EPOCH;

  return days;
}

#undef YEARS_FROM_0001_01_01_TO_1900
#undef DAYS_FROM_0001_01_01_TO_EPOCH

// -----------------------------------------------------------------------------

static SEXP posixct_get_week_offset(SEXP x);
static SEXP posixlt_get_week_offset(SEXP x);

// [[ "utils.h" ]]
SEXP get_week_offset(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return date_get_week_offset(x);
  case warp_class_posixct: return posixct_get_week_offset(x);
  case warp_class_posixlt: return posixlt_get_week_offset(x);
  default: r_error("get_week_offset", "Internal error: Unknown date time class.");
  }
}

static SEXP posixct_get_week_offset(SEXP x) {
  x = PROTECT(as_posixlt_from_posixct(x));
  SEXP out = posixlt_get_week_offset(x);
  UNPROTECT(1);
  return out;
}

#define WEEKS_IN_YEAR 53

static SEXP posixlt_get_week_offset(SEXP x) {
  SEXP year = VECTOR_ELT(x, 5);
  SEXP yday = VECTOR_ELT(x, 7);

  if (TYPEOF(year) != INTSXP) {
    r_error(
      "posixlt_get_week_offset",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(yday) != INTSXP) {
    r_error(
      "posixlt_get_week_offset",
      "Internal error: The 8th element of the POSIXlt object should be an integer."
    );
  }

  int* p_year = INTEGER(year);
  int* p_yday = INTEGER(yday);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_year[i] == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = (p_year[i] - 70) * WEEKS_IN_YEAR + p_yday[i] / 7;
  }

  UNPROTECT(1);
  return out;
}

#undef WEEKS_IN_YEAR
