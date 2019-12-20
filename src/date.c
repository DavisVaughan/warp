#include "utils.h"

static void days_to_year_month(int n, int* p_year, int* p_month);

// -----------------------------------------------------------------------------

static SEXP int_date_get_year(SEXP x);
static SEXP dbl_date_get_year(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_year(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_year(x);
  case REALSXP: return dbl_date_get_year(x);
  default: r_error("date_get_year", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_year(SEXP x) {
  return date_get_year(x);
}

static SEXP int_date_get_year(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  int year;
  int month;

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    days_to_year_month(elt, &year, &month);

    p_out[i] = year;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_get_year(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  int year;
  int month;

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    days_to_year_month(elt, &year, &month);

    p_out[i] = year;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP int_date_get_year_month(SEXP x);
static SEXP dbl_date_get_year_month(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_year_month(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_year_month(x);
  case REALSXP: return dbl_date_get_year_month(x);
  default: r_error("date_get_year", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_year_month(SEXP x) {
  return date_get_year_month(x);
}

static SEXP int_date_get_year_month(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP month = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_month = INTEGER(month);

  int temp_year;
  int temp_month;

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_year[i] = NA_INTEGER;
      p_month[i] = NA_INTEGER;
      continue;
    }

    days_to_year_month(elt, &temp_year, &temp_month);

    p_year[i] = temp_year;
    p_month[i] = temp_month;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(3);
  return out;
}

static SEXP dbl_date_get_year_month(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP month = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_month = INTEGER(month);

  int temp_year;
  int temp_month;

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_year[i] = NA_INTEGER;
      p_month[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    days_to_year_month(elt, &temp_year, &temp_month);

    p_year[i] = temp_year;
    p_month[i] = temp_month;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

// Using _ord2ymd()
// https://github.com/python/cpython/blob/b0d4949f1fb04f83691e10a5453d1e10e4598bb9/Lib/datetime.py#L87

static const int DAYS_IN_MONTH[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static const int DAYS_UP_TO_MONTH[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

#define YEAR_OFFSET_FROM_EPOCH 1970

// unclass(as.Date("0001-01-01"))
#define DAYS_FROM_0001_01_01_TO_EPOCH 719162

#define DAYS_IN_1_YEAR_CYCLE 365

// 4 * DAYS_IN_1_YEAR_CYCLE + 1
#define DAYS_IN_4_YEAR_CYCLE 1461

// 25 * DAYS_IN_4_YEAR_CYCLE - 1
#define DAYS_IN_100_YEAR_CYCLE 36524

// 4 * DAYS_IN_100_YEAR_CYCLE + 1
#define DAYS_IN_400_YEAR_CYCLE 146097

static void divmod(int x, int y, int* p_quot, int* p_rem);

// -----------------------------------------------------------------------------

static void days_to_year_month(int n, int* p_year, int* p_month) {
  int n_1_year_cycles;
  int n_4_year_cycles;
  int n_100_year_cycles;
  int n_400_year_cycles;

  int year;
  int month;

  // Adjust to be days since 0001-01-01
  n = DAYS_FROM_0001_01_01_TO_EPOCH + n;

  divmod(n, DAYS_IN_400_YEAR_CYCLE, &n_400_year_cycles, &n);
  divmod(n, DAYS_IN_100_YEAR_CYCLE, &n_100_year_cycles, &n);
  divmod(n, DAYS_IN_4_YEAR_CYCLE, &n_4_year_cycles, &n);
  divmod(n, DAYS_IN_1_YEAR_CYCLE, &n_1_year_cycles, &n);

  year = 1 +
    n_400_year_cycles * 400 +
    n_100_year_cycles * 100 +
    n_4_year_cycles * 4 +
    n_1_year_cycles;

  if (n_1_year_cycles == 4 || n_100_year_cycles == 4) {
    *p_year = (year - 1) - YEAR_OFFSET_FROM_EPOCH;
    *p_month = 12 - 1;
    return;
  }

  bool is_leap_year = (n_1_year_cycles == 3) &&
    (n_4_year_cycles != 24 || n_100_year_cycles == 3);

  month = (n + 50) >> 5;

  // Number of days up to this month
  int preceding = DAYS_UP_TO_MONTH[month - 1] + (month > 2 && is_leap_year);

  // "guess" of `month` was 1 too large, so subtract off the number
  // of days in the extra month
  if (preceding > n) {
    --month;
    preceding -= DAYS_IN_MONTH[month - 1] + (month == 2 && is_leap_year);
  }

  // To be complete, but we don't need it
  // n -= preceding;
  // n++;

  *p_year = year - YEAR_OFFSET_FROM_EPOCH;
  *p_month = month - 1;

  return;
}

#undef YEAR_OFFSET_FROM_EPOCH

#undef DAYS_FROM_0001_01_01_TO_EPOCH

#undef DAYS_IN_1_YEAR_CYCLE
#undef DAYS_IN_4_YEAR_CYCLE
#undef DAYS_IN_100_YEAR_CYCLE
#undef DAYS_IN_400_YEAR_CYCLE

// -----------------------------------------------------------------------------

// Careful to use "floor division" like Python's divmod().
// C's div() is not the same!
// Critical for negative dates before 0001-01-01
// https://stackoverflow.com/questions/3895081/divide-and-get-remainder-at-the-same-time

static void divmod(int x, int y, int* p_quot, int* p_rem) {
  int quot, rem;

  if (y == 0) {
    Rf_errorcall(R_NilValue, "Division by zero!");
  }

  quot = x / y;
  rem = (int)(x - (unsigned int)quot * y);

  // i.e. and signs differ
  if (rem && ((y ^ rem) < 0)) {
    rem += y;
    --quot;
  }

  *p_quot = quot;
  *p_rem = rem;
}
