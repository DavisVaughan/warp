#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP warp_change_impl(SEXP x, bool last, bool endpoint);

// [[ include("warp.h") ]]
SEXP warp_change(SEXP x,
                 enum warp_period_type period,
                 int every,
                 SEXP origin,
                 bool last,
                 bool endpoint) {
  SEXP distances = PROTECT(warp_distance(x, period, every, origin));
  SEXP out = warp_change_impl(distances, last, endpoint);
  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP warp_warp_change(SEXP x,
                      SEXP period,
                      SEXP every,
                      SEXP origin,
                      SEXP last,
                      SEXP endpoint) {
  enum warp_period_type period_ = as_period_type(period);
  int every_ = pull_every(every);
  bool last_ = pull_last(last);
  bool endpoint_ = pull_endpoint(endpoint);
  return warp_change(x, period_, every_, origin, last_, endpoint_);
}

// -----------------------------------------------------------------------------

static inline bool dbl_equal(const double current, const double previous);

static SEXP warp_change_impl(SEXP x, bool last, bool endpoint) {
  const R_xlen_t size = Rf_xlength(x);

  if (size == 0) {
    return Rf_allocVector(REALSXP, 0);
  }
  if (size == 1) {
    return Rf_ScalarReal(1);
  }

  R_xlen_t count = 0;

  // Maximum size is if all values are unique
  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  const double* p_x = REAL(x);

  if (last) {
    // If the location of the first changepoint
    // wasn't the first location in `x`, we need to forcibly add the endpoint
    if (endpoint && dbl_equal(p_x[0], p_x[1])) {
      p_out[count] = 1;
      ++count;
    }
  } else {
    // Always include first value when returning starts
    p_out[count] = 1;
    ++count;
  }

  const R_xlen_t adjustment = (R_xlen_t) !last;

  double previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const double current = p_x[i];

    if (dbl_equal(current, previous)) {
      continue;
    }

    const R_xlen_t loc = i + adjustment;

    p_out[count] = loc;

    ++count;
    previous = current;
  }

  if (last) {
    // Always include last value when returning stops
    p_out[count] = size;
    ++count;
  } else {
    // If the location of the last changepoint
    // wasn't the last location in `x`, we need to forcibly add the endpoint
    if (endpoint && dbl_equal(p_x[size - 2], p_x[size - 1])) {
      p_out[count] = size;
      ++count;
    }
  }

  out = PROTECT(Rf_xlengthgets(out, count));

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
