#include "utils.h"

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

static SEXP int_date_get_year_month_offset(SEXP x);
static SEXP dbl_date_get_year_month_offset(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_year_month_offset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_year_month_offset(x);
  case REALSXP: return dbl_date_get_year_month_offset(x);
  default: r_error("date_get_year_month_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_year_month_offset(SEXP x) {
  return date_get_year_month_offset(x);
}

static SEXP int_date_get_year_month_offset(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP month = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_month = INTEGER(month);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_year[i] = NA_INTEGER;
      p_month[i] = NA_INTEGER;
      continue;
    }

    struct warp_components components = convert_days_to_components(elt);

    p_year[i] = components.year;
    p_month[i] = components.month;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(3);
  return out;
}

static SEXP dbl_date_get_year_month_offset(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP month = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_month = INTEGER(month);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_year[i] = NA_INTEGER;
      p_month[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    struct warp_components components = convert_days_to_components(elt);

    p_year[i] = components.year;
    p_month[i] = components.month;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP int_date_get_year_yday_offset(SEXP x);
static SEXP dbl_date_get_year_yday_offset(SEXP x);

// [[ include("utils.h") ]]
SEXP date_get_year_yday_offset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_get_year_yday_offset(x);
  case REALSXP: return dbl_date_get_year_yday_offset(x);
  default: r_error("date_get_year_yday_offset", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP warp_date_get_year_yday_offset(SEXP x) {
  return date_get_year_yday_offset(x);
}

static SEXP int_date_get_year_yday_offset(SEXP x) {
  int* p_x = INTEGER(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP yday = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_yday = INTEGER(yday);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_year[i] = NA_INTEGER;
      p_yday[i] = NA_INTEGER;
      continue;
    }

    struct warp_components components = convert_days_to_components(elt);

    p_year[i] = components.year;
    p_yday[i] = components.yday;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, yday);

  UNPROTECT(3);
  return out;
}

static SEXP dbl_date_get_year_yday_offset(SEXP x) {
  double* p_x = REAL(x);

  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP yday = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_yday = INTEGER(yday);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_year[i] = NA_INTEGER;
      p_yday[i] = NA_INTEGER;
      continue;
    }

    // Truncate fractional pieces towards 0
    int elt = x_elt;

    struct warp_components components = convert_days_to_components(elt);

    p_year[i] = components.year;
    p_yday[i] = components.yday;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, yday);

  UNPROTECT(3);
  return out;
}

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

static void divmod(int x, int y, int* p_quot, int* p_rem);

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

  components.yday = n;

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
    return components;
  }

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

// -----------------------------------------------------------------------------

/*
 * `divmod()`
 *
 * `divmod()` is equivalent to `div()`, except in the important case where the
 * signs of `x` and `y` differ. Using floating point division in these cases
 * would generate a negative quotient. `divmod()` always rounds this quotient
 * "down" towards -Inf, rather than towards 0 like `div()` does. The remainder
 * is then computed from that, and it always works out that it has the same
 * sign as `y`.
 *
 * `divmod()` is useful for the calculations when computing the year/month/day
 * components. For example, with `n = -5L`, which is 5 days before `2001-01-01`,
 * for the first computation we would get:
 * [-1, 146092] = divmod(-5L, DAYS_IN_400_YEAR_CYCLE)
 * which is telling us that we are somewhere between the 0th and -1st 400 year
 * cycle. So somewhere between [1601-01-01, 2001-01-01). And we are at the
 * 146092th day in the cycle. So then we repeat for a 100 year cycle, 4 year
 * cycle, and 1 year cycle to finally end up with an `n` that tells us the
 * position in the year. Then we add that all back together to get the correct
 * year (with the one edge case that we might be at a 4 year or 400 year
 * boundary, like what `n = -1L` would give). The rest of the computation is
 * then about finding the month in that year, using an educated guess of
 * `(n + 50) >> 5` which is either exactly right or one too far.
 *
 * In the technical sense, it is defined as:
 *
 * - quot = x // y
 * - rem = x % y
 * - Where this holds: `(x // y) * y + (x % y) == x`
 * - Where `(x % y)` has the same sign as `y`
 * - Integer division of `(x // y)` always "rounds down" towards -Inf, not
 *   towards 0.
 *
 * To compute it "by hand" the easiest way is to:
 *
 * 1) Compute the quotient as:
 *    `quot = x // y = floor( (double) x, (double) y)`
 *    Where `floor()` always rounds towards -Inf. So `floor(-0.5) = -1`.
 * 2) Compute the remainder by backsolving the invariant:
 *    `rem = x % y = x - (x // y) * y = x - (quot) * y`
 *
 * This is in contrast to `div()` which would use the exact same procedure as
 * above but would substitute `floor()` for `trunc()`, so `trunc(-0.5) = 0`.
 * Because the remainder is then computed from that, it would also change.
 *
 * @param x
 *   The numerator.
 * @param y
 *   The denominator.
 * @param p_quot
 *   A pointer to place the quotient value of the division in.
 * @param p_rem
 *   A pointer to place the remainder value of the division in.
 */

/*
 * Examples of `div()` vs `divmod()`
 *
 * divmod:
 * 1) quot = 1 // 2 = floor(1.0 / 2.0) = floor(0.5) = 0
 * 2) rem = 1 % 2 = 1 - (0) * 2 = 1
 *
 * div:
 * 1) quot = 1 / 2 = trunc(1.0 / 2.0) = trunc(0.5) = 0
 * 2) rem = 1 % 2 = 1 - (0) * 2 = 1
 *
 * [0, 1] = divmod(1L, 2L)
 * [0, 1] = div(1L, 2L)
 *
 * divmod:
 * 1) quot = -1 // 2 = floor(-1.0 / 2.0) = floor(-0.5) = -1
 * 2) rem = -1 % 2 = -1 - (-1) * 2 = 1
 *
 * div:
 * 1) quot = -1 / 2 = trunc(-1.0 / 2.0) = trunc(-0.5) = 0
 * 2) rem = -1 % 2 = -1 - (0) * 2 = -1
 *
 * [-1,  1] = divmod(-1L, 2L)
 * [ 0, -1] = div(-1L, 2L)
 *
 * divmod:
 * 1) quot = 1 // -2 = floor(1.0 / -2.0) = floor(-0.5) = -1
 * 2) rem = 1 % -2 = 1 - (-1) * -2 = -1
 *
 * div:
 * 1) quot = 1 / -2 = trunc(1.0 / -2.0) = trunc(-0.5) = 0
 * 2) rem = 1 % -2 = 1 - (0) * -2 = 1
 *
 * [-1, -1] = divmod(1L, -2L)
 * [ 0,  1] = div(1L, -2L)
 *
 * divmod:
 * 1) quot = -1 // -2 = floor(-1.0 / -2.0) = floor(0.5) = 0
 * 2) rem = -1 % -2 = -1 - (0) * -2 = -1
 *
 * div:
 * 1) quot = -1 / -2 = trunc(-1.0 / -2.0) = trunc(0.5) = 0
 * 2) rem = -1 % -2 = -1 - (0) * -2 = -1
 *
 * [0, -1] = divmod(-1L, -2L)
 * [0, -1] = div(-1L, -2L)
 */

/*
 * Python's i_divmod()
 * http://svn.python.org/projects/python/trunk/Objects/intobject.c
 *
 * Implemented after finding this Stack Overflow answer
 * https://stackoverflow.com/questions/3895081/divide-and-get-remainder-at-the-same-time
 *
 * The notes below are all derived from the comments in the Python implementation
 */

/*
 * It is possible for `x - quot * y` to overflow on platforms where `x / y`
 * gives `floor(x / y)` and does not truncate towards 0. This is rare, and C99
 * prohibits it, but it is technically possible on C89. One example would be:
 * x = INT_MIN = -.Machine$integer.max
 * y = 2L
 *
 * If this problematic behavior was in force, it would give:
 * quot = x / y = floor(-1073741823.5) = -1073741824
 * quot * y = -2147483648 > INT_MIN so overflow
 *
 * With truncation towards 0 we get:
 * quot = x / y = trunc(-1073741823.5) = -1073741823
 * quot * y = -2147483646 < INT_MIN so no overflow
 *
 * I do not expect any issues because R uses C99, but to be safe we cast
 * `quot` to an unsigned int in the middle to avoid overflow, then cast back
 * to int after the subtraction.
 */

/*
 * If the signs of `x` and `y` are different, and there is a non-0 remainder,
 * we generally expect that we will need to perform the adjustment to the
 * remainder and quotient to get the correct `divmod()` results.
 *
 * However, as mentioned above, C89 does not define whether `x / y` gives the
 * `floor(x / y)` or `trunc(x / y)`. If it happens to return the non-standard
 * result of `floor(x / y)`, then we don't need to perform the adjustment
 * because the result will already be correct. Because of this, we can't check
 * the sign of `x` and `y` directly.
 *
 * The universal trick to know if we need to perform the adjustment is to look
 * at the `rem` value and check if its sign is the same as `y`. If the signs
 * are different we need the adjustment. This works on problematic C89 platforms
 * and C99 and above.
 */

static void divmod(int x, int y, int* p_quot, int* p_rem) {
  if (y == 0) {
    Rf_errorcall(R_NilValue, "Division by zero is not allowed.");
  }

  int quot = x / y;
  int rem = (int)(x - (unsigned int)quot * y);

  if (rem && ((y ^ rem) < 0)) {
    rem += y;
    --quot;
  }

  *p_quot = quot;
  *p_rem = rem;
}

// Exposed for testing
// [[ register() ]]
SEXP warp_divmod(SEXP x, SEXP y) {
  int x_ = INTEGER(x)[0];
  int y_ = INTEGER(y)[0];

  int quot;
  int rem;

  divmod(x_, y_, &quot, &rem);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));

  INTEGER(out)[0] = quot;
  INTEGER(out)[1] = rem;

  UNPROTECT(1);
  return out;
}

// Exposed for testing
// [[ register() ]]
SEXP warp_div(SEXP x, SEXP y) {
  int x_ = INTEGER(x)[0];
  int y_ = INTEGER(y)[0];

  div_t result = div(x_, y_);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));

  INTEGER(out)[0] = result.quot;
  INTEGER(out)[1] = result.rem;

  UNPROTECT(1);
  return out;
}
