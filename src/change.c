#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP warp_change_impl(SEXP x);

// [[ include("warp.h") ]]
SEXP warp_change(SEXP x, enum warp_period_type type, int every, SEXP origin) {
  SEXP distances = PROTECT(warp_distance(x, type, every, origin));
  SEXP out = warp_change_impl(distances);
  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP warp_warp_change(SEXP x, SEXP period, SEXP every, SEXP origin) {
  enum warp_period_type type = as_period_type(period);
  int every_ = pull_every(every);
  return warp_change(x, type, every_, origin);
}

// -----------------------------------------------------------------------------

static inline bool dbl_equal(const double current, const double previous);

static SEXP warp_change_impl(SEXP x) {
  R_xlen_t size = Rf_xlength(x);

  if (size == 0) {
    return Rf_allocVector(REALSXP, 0);
  }

  if (size == 1) {
    return Rf_ScalarReal(1);
  }

  int count = 0;
  int pos_last = 0;

  // Maximum size is if all values are unique
  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  const double* p_x = REAL(x);

  double previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const double current = p_x[i];

    if (dbl_equal(current, previous)) {
      continue;
    }

    // R indexed, and really `- 1 + 1`
    p_out[count] = i;

    count++;
    pos_last = i;
    previous = current;
  }

  // Always include the last value
  if (pos_last != size) {
    p_out[count] = size;
    count++;
  }

  out = PROTECT(Rf_lengthgets(out, count));

  UNPROTECT(2);
  return out;
}

// Because the values come from `warp_distance()`, we can be confident that
// they are doubles, possibly `NA_real_` (but not `NaN` or `Inf`!)

// Order of checks
// - If `current` is `NA_real_`, check if `previous` is too
// - Otherwise just check the values

static inline bool dbl_equal(const double current, const double previous) {
  if (isnan(current)) {
    if (isnan(previous)) {
      return true;
    } else {
      return false;
    }
  }

  return current == previous;
}
