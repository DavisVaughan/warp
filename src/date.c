#include "utils.h"
#include "divmod.h"

/*
 * This file implements a VERY fast getter for year and year-month offsets for
 * a Date object. It does not go through POSIXlt, and uses an algorithm from
 * Python's datetime library for the computation of the year and month
 * components. It is both much faster and highly memory efficient.
 */

/*
 * @member year
 *   The year offset. The number of years since 1970.
 * @member month
 *   The month. Mapped to the range of 0-11, where 0 is January.
 * @member day
 *   The day of month. Mapped to the range of 0-30.
 * @member yday
 *   The day of the year. Mapped to the range of 0-365.
 */
struct warp_components {
  int year;
  int month;
  int day;
  int yday;
};

static struct warp_components convert_days_to_components(int n);

// -----------------------------------------------------------------------------

static SEXP int_date_get_year_offset(SEXP x);
static SEXP dbl_date_get_year_offset(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_year_offset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_year_offset(x);
  case REALSXP: return dbl_date_get_year_offset(x);
  default: r_error("date_get_year_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_year_offset(SEXP x) {
  return date_get_year_offset(x);
}

static SEXP int_date_get_year_offset(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    struct warp_components components = convert_days_to_components(elt);

    p_out[i] = components.year;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_get_year_offset(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    struct warp_components components = convert_days_to_components(elt);

    p_out[i] = components.year;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP int_date_get_month_offset(SEXP x);
static SEXP dbl_date_get_month_offset(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_month_offset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_month_offset(x);
  case REALSXP: return dbl_date_get_month_offset(x);
  default: r_error("date_get_month_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_month_offset(SEXP x) {
  return date_get_month_offset(x);
}

static SEXP int_date_get_month_offset(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    struct warp_components components = convert_days_to_components(elt);

    p_out[i] = components.year * 12 + components.month;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_get_month_offset(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    struct warp_components components = convert_days_to_components(elt);

    p_out[i] = components.year * 12 + components.month;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP int_date_get_yday_offset(SEXP x, int every);
static SEXP dbl_date_get_yday_offset(SEXP x, int every);

// [[ include("utils.h") ]]
SEXP date_get_yday_offset(SEXP x, int every) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_yday_offset(x, every);
  case REALSXP: return dbl_date_get_yday_offset(x, every);
  default: r_error("date_get_yday_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

#define DAYS_IN_YEAR 365
#define DAYS_IN_LEAP_YEAR 366

static SEXP int_date_get_yday_offset(SEXP x, int every) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  int units_in_non_leap_year = (DAYS_IN_YEAR - 1) / every + 1;
  int units_in_leap_year = (DAYS_IN_LEAP_YEAR - 1) / every + 1;

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    struct warp_components components = convert_days_to_components(elt);

    int day_units_before_year = units_before_year(
      components.year,
      units_in_non_leap_year,
      units_in_leap_year
    );

    p_out[i] = day_units_before_year + components.yday / every;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_get_yday_offset(SEXP x, int every) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  int units_in_non_leap_year = (DAYS_IN_YEAR - 1) / every + 1;
  int units_in_leap_year = (DAYS_IN_LEAP_YEAR - 1) / every + 1;

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    struct warp_components components = convert_days_to_components(elt);

    int day_units_before_year = units_before_year(
      components.year,
      units_in_non_leap_year,
      units_in_leap_year
    );

    p_out[i] = day_units_before_year + components.yday / every;
  }

  UNPROTECT(1);
  return out;
}

#undef DAYS_IN_YEAR
#undef DAYS_IN_LEAP_YEAR

// -----------------------------------------------------------------------------

static const int DAYS_IN_MONTH[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static const int DAYS_UP_TO_MONTH[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

#define YEAR_OFFSET_FROM_EPOCH 30

#define MONTH_ADJUSTMENT_TO_0_TO_11_RANGE -1
#define DAY_ADJUSTMENT_TO_0_TO_30_RANGE -1

// unclass(as.Date("2001-01-01"))
#define DAYS_FROM_2001_01_01_TO_EPOCH -11323

// -.Machine$integer.max - (DAYS_FROM_2001_01_01_TO_EPOCH)
// -.Machine$integer.max == (INT_MIN + 1)
#define SMALLEST_POSSIBLE_DAYS_FROM_EPOCH (INT_MIN + 1 - DAYS_FROM_2001_01_01_TO_EPOCH)

#define DAYS_IN_1_YEAR_CYCLE 365

// 4 * DAYS_IN_1_YEAR_CYCLE + 1
#define DAYS_IN_4_YEAR_CYCLE 1461

// 25 * DAYS_IN_4_YEAR_CYCLE - 1
#define DAYS_IN_100_YEAR_CYCLE 36524

// 4 * DAYS_IN_100_YEAR_CYCLE + 1
#define DAYS_IN_400_YEAR_CYCLE 146097

// -----------------------------------------------------------------------------

/*
 * `convert_days_to_components()`
 *
 * @param n
 *   A 0-based number of days since 1970-01-01, i.e. unclass(<Date>).
 */

/*
 * Python's datetime `_ord2ymd()`
 * https://github.com/python/cpython/blob/b0d4949f1fb04f83691e10a5453d1e10e4598bb9/Lib/datetime.py#L87
 *
 * Many of the comments are copied over from this function.
 */

/*
 * The challenging thing about finding the year/month is the presence of leap
 * years. The leap year pattern repeats exactly every 400 years. We adjust the
 * date to be the number of days since 2001-01-01 because that is the closest
 * 400 year boundary to 1970-01-01. This is important because it gives us the
 * maximum amount of values before hitting any integer overflow.
 *
 * The basic strategy is to find the closest 400 year boundary at or _before_
 * `n`, and then work with the offset (in number of days) from that boundary to
 * `n`. It is further divided into 100 / 4 / 1 year cycles, which reduce `n`
 * down to the "day of the year" in the year of interest. We compute the actual
 * year from the number of 400 / 100 / 4 / 1 year cycles, with an adjustment if
 * we are exactly on a 4 or 400 year boundary. Then the rest of the code is
 * dedicated to finding the month. There is an "educated guess" of
 * `(n + 50) >> 5` that gets us either exactly right or 1 too far. If we are too
 * far, we adjust it back by 1 month.
 */

static struct warp_components convert_days_to_components(int n) {
  struct warp_components components;

  int n_1_year_cycles;
  int n_4_year_cycles;
  int n_100_year_cycles;
  int n_400_year_cycles;

  // The smallest possible value of `n` before overflow from
  // addition of DAYS_FROM_2001_01_01_TO_EPOCH
  if (n < SMALLEST_POSSIBLE_DAYS_FROM_EPOCH) {
    r_error(
      "convert_days_to_components",
      "Integer overflow! "
      "The smallest possible value for `n` is %i",
      SMALLEST_POSSIBLE_DAYS_FROM_EPOCH
    );
  }

  // Adjust to be days since 2001-01-01 (so `n = 0 == 2001-01-01`)
  n = DAYS_FROM_2001_01_01_TO_EPOCH + n;

  divmod(n, DAYS_IN_400_YEAR_CYCLE, &n_400_year_cycles, &n);
  divmod(n, DAYS_IN_100_YEAR_CYCLE, &n_100_year_cycles, &n);
  divmod(n, DAYS_IN_4_YEAR_CYCLE, &n_4_year_cycles, &n);
  divmod(n, DAYS_IN_1_YEAR_CYCLE, &n_1_year_cycles, &n);

  int year = 1 +
    n_400_year_cycles * 400 +
    n_100_year_cycles * 100 +
    n_4_year_cycles * 4 +
    n_1_year_cycles;

  // Edge case adjustment required if we are on the border of a
  // 4 year or 400 year cycle boundary (i.e. `n = -1L`)
  if (n_1_year_cycles == 4 || n_100_year_cycles == 4) {
    components.year = (year - 1) + YEAR_OFFSET_FROM_EPOCH;
    components.month = 12 + MONTH_ADJUSTMENT_TO_0_TO_11_RANGE;
    components.day = 31 + DAY_ADJUSTMENT_TO_0_TO_30_RANGE;
    components.yday = 365;
    return components;
  }

  components.yday = n;

  bool is_leap_year = (n_1_year_cycles == 3) &&
    (n_4_year_cycles != 24 || n_100_year_cycles == 3);

  // Gets us either exactly right, or 1 month too far
  int month = (n + 50) >> 5;

  // Number of days up to this month, computed using our `month` guess
  int preceding = DAYS_UP_TO_MONTH[month - 1] + (is_leap_year && month > 2);

  // If the number of `preceding` days is greater than the `n` yday
  // position in the year, then we obviously went too far. So subtract 1
  // month and recompute the number of days up to the (now correct) month.
  if (preceding > n) {
    --month;
    preceding -= DAYS_IN_MONTH[month - 1] + (is_leap_year && month == 2);
  }

  // Substract `position in year` - `days up to current month` = `day in month`
  // It will be 0-30 based already
  n -= preceding;

  components.year = year + YEAR_OFFSET_FROM_EPOCH;
  components.month = month + MONTH_ADJUSTMENT_TO_0_TO_11_RANGE;
  components.day = n;

  return components;
}

#undef YEAR_OFFSET_FROM_EPOCH

#undef MONTH_ADJUSTMENT_TO_0_TO_11_RANGE
#undef DAY_ADJUSTMENT_TO_0_TO_30_RANGE

#undef DAYS_FROM_2001_01_01_TO_EPOCH

#undef SMALLEST_POSSIBLE_DAYS_FROM_EPOCH

#undef DAYS_IN_1_YEAR_CYCLE
#undef DAYS_IN_4_YEAR_CYCLE
#undef DAYS_IN_100_YEAR_CYCLE
#undef DAYS_IN_400_YEAR_CYCLE

// -----------------------------------------------------------------------------

// [[ export() ]]
SEXP warp_convert_days_to_components(SEXP n) {
  int n_ = INTEGER(n)[0];

  struct warp_components components = convert_days_to_components(n_);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, 4));

  INTEGER(out)[0] = components.year;
  INTEGER(out)[1] = components.month;
  INTEGER(out)[2] = components.day;
  INTEGER(out)[3] = components.yday;

  UNPROTECT(1);
  return out;
}
