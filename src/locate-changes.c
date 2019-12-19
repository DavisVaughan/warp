#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// Order of checks
// - If `current` is a number, are the both the same number?
// - `current` is not a number. Are they both the same missing type?

static inline bool dbl_equal(const double current, const double previous) {
  enum warp_dbl_class current_dbl_class = dbl_classify(current);

  if (current_dbl_class == warp_dbl_number) {
    return current == previous;
  }

  enum warp_dbl_class previous_dbl_class = dbl_classify(previous);

  return current_dbl_class == previous_dbl_class;
}

// No need to handle `NA_integer_` specially because there is only 1
// missing type for integers

static inline bool int_equal(const int current, const int previous) {
  return current == previous;
}

// -----------------------------------------------------------------------------

#define CHANGES_LOOP(CTYPE, CONST_DEREF, EQUAL_SCALAR) { \
  const CTYPE* p_x = CONST_DEREF(x);                     \
                                                         \
  CTYPE previous = p_x[0];                               \
                                                         \
  for (R_xlen_t i = 1; i < size; ++i) {                  \
    const CTYPE current = p_x[i];                        \
                                                         \
    if (EQUAL_SCALAR(current, previous)) {               \
      continue;                                          \
    }                                                    \
                                                         \
    /* R indexed, and really `- 1 + 1` */                \
    p_out[count] = i;                                    \
                                                         \
    count++;                                             \
    pos_last = i;                                        \
    previous = current;                                  \
  }                                                      \
}

// [[ include("warp.h") ]]
SEXP locate_changes(SEXP x) {
  SEXPTYPE type = TYPEOF(x);

  if (type != INTSXP && type != REALSXP) {
    r_error("locate_changes", "`x` must be an integer or double vector.");
  }

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

  switch (type) {
  case INTSXP: CHANGES_LOOP(int, INTEGER_RO, int_equal); break;
  case REALSXP: CHANGES_LOOP(double, REAL_RO, dbl_equal); break;
  default: r_error("locate_changes", "Internal error: should have caught this earlier.");
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

// [[ register() ]]
SEXP warp_locate_changes(SEXP x) {
  return locate_changes(x);
}
