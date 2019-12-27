#include "divmod.h"

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

void divmod(int x, int y, int* p_quot, int* p_rem) {
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

int int_div(int x, int y) {
  int quot;
  int rem;

  divmod(x, y, &quot, &rem);

  return quot;
}

// -----------------------------------------------------------------------------

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
