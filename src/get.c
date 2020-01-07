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
    if (!R_FINITE(p_x[i])) {
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

static struct warp_yday_components posixct_get_origin_yday_components(SEXP origin);
static struct warp_yday_components posixlt_get_origin_yday_components(SEXP origin);

// [[ include("utils.h") ]]
struct warp_yday_components get_origin_yday_components(SEXP origin) {
  if (origin == R_NilValue) {
    struct warp_yday_components out;
    out.year_offset = 0;
    out.yday = 0;
    return out;
  }

  switch(time_class_type(origin)) {
  case warp_class_date: return date_get_origin_yday_components(origin);
  case warp_class_posixct: return posixct_get_origin_yday_components(origin);
  case warp_class_posixlt: return posixlt_get_origin_yday_components(origin);
  default: r_error("get_origin_yday_components", "Internal error: Unknown date time class.");
  }
}

static struct warp_yday_components posixct_get_origin_yday_components(SEXP origin) {
  origin = PROTECT(as_posixlt_from_posixct(origin));
  struct warp_yday_components out = posixlt_get_origin_yday_components(origin);
  UNPROTECT(1);
  return out;
}

static struct warp_yday_components posixlt_get_origin_yday_components(SEXP origin) {
  SEXP origin_year = VECTOR_ELT(origin, 5);
  SEXP origin_yday = VECTOR_ELT(origin, 7);

  if (TYPEOF(origin_year) != INTSXP) {
    r_error(
      "posixlt_get_origin_yday_components",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(origin_yday) != INTSXP) {
    r_error(
      "posixlt_get_origin_yday_components",
      "Internal error: The 8th element of the POSIXlt object should be an integer."
    );
  }

  int year = INTEGER(origin_year)[0];
  int yday = INTEGER(origin_yday)[0];

  if (year == NA_INTEGER || yday == NA_INTEGER) {
    r_error(
      "posixlt_get_origin_yday_components",
      "The `origin` cannot be `NA`."
    );
  }

  struct warp_yday_components out;

  out.year_offset = year - 70;
  out.yday = yday;

  return out;
}

// -----------------------------------------------------------------------------

static struct warp_mday_components posixct_get_origin_mday_components(SEXP origin);
static struct warp_mday_components posixlt_get_origin_mday_components(SEXP origin);

// [[ include("utils.h") ]]
struct warp_mday_components get_origin_mday_components(SEXP origin) {
  if (origin == R_NilValue) {
    struct warp_mday_components out;
    out.year_offset = 0;
    out.month = 0;
    return out;
  }

  switch(time_class_type(origin)) {
  case warp_class_date: return date_get_origin_mday_components(origin);
  case warp_class_posixct: return posixct_get_origin_mday_components(origin);
  case warp_class_posixlt: return posixlt_get_origin_mday_components(origin);
  default: r_error("get_origin_mday_components", "Internal error: Unknown date time class.");
  }
}

static struct warp_mday_components posixct_get_origin_mday_components(SEXP origin) {
  origin = PROTECT(as_posixlt_from_posixct(origin));
  struct warp_mday_components out = posixlt_get_origin_mday_components(origin);
  UNPROTECT(1);
  return out;
}

static struct warp_mday_components posixlt_get_origin_mday_components(SEXP origin) {
  SEXP origin_year = VECTOR_ELT(origin, 5);
  SEXP origin_month = VECTOR_ELT(origin, 4);

  if (TYPEOF(origin_year) != INTSXP) {
    r_error(
      "posixlt_get_origin_mday_components",
      "Internal error: The 6th element of the POSIXlt object should be an integer."
    );
  }

  if (TYPEOF(origin_month) != INTSXP) {
    r_error(
      "posixlt_get_origin_mday_components",
      "Internal error: The 4th element of the POSIXlt object should be an integer."
    );
  }

  int year = INTEGER(origin_year)[0];
  int month = INTEGER(origin_month)[0];

  if (year == NA_INTEGER || month == NA_INTEGER) {
    r_error(
      "posixlt_get_origin_mday_components",
      "The `origin` cannot be `NA`."
    );
  }

  struct warp_mday_components out;

  out.year_offset = year - 70;
  out.month = month;

  return out;
}
