#include "warp.h"

// -----------------------------------------------------------------------------

static bool pull_strictly(SEXP strictly);
static bool pull_increasing(SEXP increasing);
static bool pull_na_largest(SEXP na_value);

static bool dbl_is_sorted_strictly_increasing_na_largest(SEXP x, R_xlen_t size);
static bool int_is_sorted_strictly_increasing_na_largest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_strictly_increasing_na_smallest(SEXP x, R_xlen_t size);
static bool int_is_sorted_strictly_increasing_na_smallest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_nonstrictly_increasing_na_largest(SEXP x, R_xlen_t size);
static bool int_is_sorted_nonstrictly_increasing_na_largest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_nonstrictly_increasing_na_smallest(SEXP x, R_xlen_t size);
static bool int_is_sorted_nonstrictly_increasing_na_smallest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_strictly_decreasing_na_largest(SEXP x, R_xlen_t size);
static bool int_is_sorted_strictly_decreasing_na_largest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_strictly_decreasing_na_smallest(SEXP x, R_xlen_t size);
static bool int_is_sorted_strictly_decreasing_na_smallest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_nonstrictly_decreasing_na_largest(SEXP x, R_xlen_t size);
static bool int_is_sorted_nonstrictly_decreasing_na_largest(SEXP x, R_xlen_t size);

static bool dbl_is_sorted_nonstrictly_decreasing_na_smallest(SEXP x, R_xlen_t size);
static bool int_is_sorted_nonstrictly_decreasing_na_smallest(SEXP x, R_xlen_t size);

// -----------------------------------------------------------------------------

SEXP warp_is_sorted(SEXP x, bool strictly, bool increasing, bool na_largest) {
  SEXPTYPE type = TYPEOF(x);

  if (type != REALSXP && type != INTSXP) {
    Rf_errorcall(R_NilValue, "`x` must be an integer or a double.");
  }

  R_xlen_t size = Rf_xlength(x);

  if (size == 0 || size == 1) {
    return Rf_ScalarLogical(1);
  }

  bool out;

  switch (type) {
  case REALSXP: {
    if (strictly) {
      if (increasing) {
        if (na_largest) {
          out = dbl_is_sorted_strictly_increasing_na_largest(x, size);
        } else {
          out = dbl_is_sorted_strictly_increasing_na_smallest(x, size);
        }
      } else {
        if (na_largest) {
          out = dbl_is_sorted_strictly_decreasing_na_largest(x, size);
        } else {
          out = dbl_is_sorted_strictly_decreasing_na_smallest(x, size);
        }
      }
    } else {
      if (increasing) {
        if (na_largest) {
          out = dbl_is_sorted_nonstrictly_increasing_na_largest(x, size);
        } else {
          out = dbl_is_sorted_nonstrictly_increasing_na_smallest(x, size);
        }
      } else {
        if (na_largest) {
          out = dbl_is_sorted_nonstrictly_decreasing_na_largest(x, size);
        } else {
          out = dbl_is_sorted_nonstrictly_decreasing_na_smallest(x, size);
        }
      }
    }
    break;
  }
  case INTSXP: {
    if (strictly) {
      if (increasing) {
        if (na_largest) {
          out = int_is_sorted_strictly_increasing_na_largest(x, size);
        } else {
          out = int_is_sorted_strictly_increasing_na_smallest(x, size);
        }
      } else {
        if (na_largest) {
          out = int_is_sorted_strictly_decreasing_na_largest(x, size);
        } else {
          out = int_is_sorted_strictly_decreasing_na_smallest(x, size);
        }
      }
    } else {
      if (increasing) {
        if (na_largest) {
          out = int_is_sorted_nonstrictly_increasing_na_largest(x, size);
        } else {
          out = int_is_sorted_nonstrictly_increasing_na_smallest(x, size);
        }
      } else {
        if (na_largest) {
          out = int_is_sorted_nonstrictly_decreasing_na_largest(x, size);
        } else {
          out = int_is_sorted_nonstrictly_decreasing_na_smallest(x, size);
        }
      }
    }
    break;
  }
  }

  return Rf_ScalarLogical(out);
}

// [[ register() ]]
SEXP warp_warp_is_sorted(SEXP x, SEXP strictly, SEXP increasing, SEXP na_value) {
  return warp_is_sorted(
    x,
    pull_strictly(strictly),
    pull_increasing(increasing),
    pull_na_largest(na_value)
  );
}

// -----------------------------------------------------------------------------

static inline bool dbl_is_na(double x) {
  return isnan(x);
}

static inline bool int_is_na(int x) {
  return x == NA_INTEGER;
}

// -----------------------------------------------------------------------------
// strictly / increasing / largest

#define IS_SORTED_STRICTLY_INCREASING_NA_LARGEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                          \
  CTYPE previous = p_x[0];                                                    \
                                                                              \
  for (R_xlen_t i = 1; i < size; ++i) {                                       \
    const CTYPE current = p_x[i];                                             \
                                                                              \
    if (IS_NA(previous)) {                                                    \
      return false;                                                           \
    }                                                                         \
                                                                              \
    if (previous >= current) {                                                \
      /* Last value is allowed to be NA */                                    \
      if (i == (size - 1) && IS_NA(current)) {                                \
        return true;                                                          \
      }                                                                       \
                                                                              \
      return false;                                                           \
    }                                                                         \
                                                                              \
    previous = current;                                                       \
  }                                                                           \
                                                                              \
  return true;                                                                \
}

static bool dbl_is_sorted_strictly_increasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_INCREASING_NA_LARGEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_strictly_increasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_INCREASING_NA_LARGEST(int, INTEGER_RO, int_is_na);
}

#undef IS_SORTED_STRICTLY_INCREASING_NA_LARGEST

// -----------------------------------------------------------------------------
// strictly / increasing / smallest

#define IS_SORTED_STRICTLY_INCREASING_NA_SMALLEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                           \
  CTYPE previous = p_x[0];                                                     \
                                                                               \
  for (R_xlen_t i = 1; i < size; ++i) {                                        \
    const CTYPE current = p_x[i];                                              \
                                                                               \
    if (IS_NA(current)) {                                                      \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (previous >= current) {                                                 \
      /* First value is allowed to be NA */                                    \
      if (i == 1 && IS_NA(previous)) {                                         \
        previous = current;                                                    \
        continue;                                                              \
      }                                                                        \
                                                                               \
      return false;                                                            \
    }                                                                          \
                                                                               \
    previous = current;                                                        \
  }                                                                            \
                                                                               \
  return true;                                                                 \
}

static bool dbl_is_sorted_strictly_increasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_INCREASING_NA_SMALLEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_strictly_increasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_INCREASING_NA_SMALLEST(int, INTEGER_RO, int_is_na);
}

#undef IS_SORTED_STRICTLY_INCREASING_NA_SMALLEST

// -----------------------------------------------------------------------------
// nonstrictly / increasing / largest

#define IS_SORTED_NONSTRICTLY_INCREASING_NA_LARGEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                             \
  CTYPE previous = p_x[0];                                                       \
  bool previous_is_na = IS_NA(previous);                                         \
                                                                                 \
  for (R_xlen_t i = 1; i < size; ++i) {                                          \
    const CTYPE current = p_x[i];                                                \
    const bool current_is_na = IS_NA(current);                                   \
                                                                                 \
    if (previous_is_na) {                                                        \
      if (current_is_na) {                                                       \
        previous = current;                                                      \
        previous_is_na = current_is_na;                                          \
        continue;                                                                \
      } else {                                                                   \
        return false;                                                            \
      }                                                                          \
    }                                                                            \
                                                                                 \
    if (previous > current) {                                                    \
      /* Catch the transition to NAs at the end */                               \
      if (current_is_na) {                                                       \
        previous = current;                                                      \
        previous_is_na = current_is_na;                                          \
        continue;                                                                \
      }                                                                          \
                                                                                 \
      return false;                                                              \
    }                                                                            \
                                                                                 \
    previous = current;                                                          \
    previous_is_na = current_is_na;                                              \
  }                                                                              \
                                                                                 \
  return true;                                                                   \
}

static bool dbl_is_sorted_nonstrictly_increasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_INCREASING_NA_LARGEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_nonstrictly_increasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_INCREASING_NA_LARGEST(int, INTEGER_RO, int_is_na);
}

#undef IS_SORTED_NONSTRICTLY_INCREASING_NA_LARGEST

// -----------------------------------------------------------------------------

#define IS_SORTED_NONSTRICTLY_INCREASING_NA_SMALLEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                              \
  CTYPE previous = p_x[0];                                                        \
  bool previous_is_na = IS_NA(previous);                                          \
                                                                                  \
  for (R_xlen_t i = 1; i < size; ++i) {                                           \
    const CTYPE current = p_x[i];                                                 \
    const bool current_is_na = IS_NA(current);                                    \
                                                                                  \
    if (current_is_na) {                                                          \
      if (previous_is_na) {                                                       \
        previous = current;                                                       \
        previous_is_na = current_is_na;                                           \
        continue;                                                                 \
      } else {                                                                    \
        return false;                                                             \
      }                                                                           \
    }                                                                             \
                                                                                  \
    if (previous > current) {                                                     \
      /* Catch transition from NA to non-NA at the start */                       \
      if (previous_is_na) {                                                       \
        previous = current;                                                       \
        previous_is_na = current_is_na;                                           \
        continue;                                                                 \
      }                                                                           \
                                                                                  \
      return false;                                                               \
    }                                                                             \
                                                                                  \
    previous = current;                                                           \
    previous_is_na = current_is_na;                                               \
  }                                                                               \
                                                                                  \
  return true;                                                                    \
}

static bool dbl_is_sorted_nonstrictly_increasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_INCREASING_NA_SMALLEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_nonstrictly_increasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_INCREASING_NA_SMALLEST(int, INTEGER_RO, int_is_na);
}

// -----------------------------------------------------------------------------

#define IS_SORTED_STRICTLY_DECREASING_NA_LARGEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                          \
                                                                              \
  CTYPE previous = p_x[0];                                                    \
                                                                              \
  for (R_xlen_t i = 1; i < size; ++i) {                                       \
    const CTYPE current = p_x[i];                                             \
                                                                              \
    if (IS_NA(current)) {                                                     \
      return false;                                                           \
    }                                                                         \
                                                                              \
    if (previous <= current) {                                                \
      /* First value is allowed to be NA */                                   \
      if (i == 1 && IS_NA(previous)) {                                        \
        previous = current;                                                   \
        continue;                                                             \
      }                                                                       \
                                                                              \
      return false;                                                           \
    }                                                                         \
                                                                              \
    previous = current;                                                       \
  }                                                                           \
                                                                              \
  return true;                                                                \
}

static bool dbl_is_sorted_strictly_decreasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_DECREASING_NA_LARGEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_strictly_decreasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_DECREASING_NA_LARGEST(int, INTEGER_RO, int_is_na);
}

// -----------------------------------------------------------------------------

#define IS_SORTED_STRICTLY_DECREASING_NA_SMALLEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                           \
  CTYPE previous = p_x[0];                                                     \
                                                                               \
  for (R_xlen_t i = 1; i < size; ++i) {                                        \
    const CTYPE current = p_x[i];                                              \
                                                                               \
    if (IS_NA(previous)) {                                                     \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (previous <= current) {                                                 \
      /* Last value is allowed to be NA */                                     \
      if (i == (size - 1) && IS_NA(current)) {                                 \
        return true;                                                           \
      }                                                                        \
                                                                               \
      return false;                                                            \
    }                                                                          \
                                                                               \
    previous = current;                                                        \
  }                                                                            \
                                                                               \
  return true;                                                                 \
}

static bool dbl_is_sorted_strictly_decreasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_DECREASING_NA_SMALLEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_strictly_decreasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_STRICTLY_DECREASING_NA_SMALLEST(int, INTEGER_RO, int_is_na);
}

// -----------------------------------------------------------------------------

#define IS_SORTED_NONSTRICTLY_DECREASING_NA_LARGEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                             \
  CTYPE previous = p_x[0];                                                       \
  bool previous_is_na = IS_NA(previous);                                         \
                                                                                 \
  for (R_xlen_t i = 1; i < size; ++i) {                                          \
    const CTYPE current = p_x[i];                                                \
    const bool current_is_na = IS_NA(current);                                   \
                                                                                 \
    if (current_is_na) {                                                         \
      if (previous_is_na) {                                                      \
        previous = current;                                                      \
        previous_is_na = current_is_na;                                          \
        continue;                                                                \
      } else {                                                                   \
        return false;                                                            \
      }                                                                          \
    }                                                                            \
                                                                                 \
    if (previous < current) {                                                    \
      /* Catch transition from NA to non-NA at the start */                      \
      if (previous_is_na) {                                                      \
        previous = current;                                                      \
        previous_is_na = current_is_na;                                          \
        continue;                                                                \
      }                                                                          \
                                                                                 \
      return false;                                                              \
    }                                                                            \
                                                                                 \
    previous = current;                                                          \
    previous_is_na = current_is_na;                                              \
  }                                                                              \
                                                                                 \
  return true;                                                                   \
}

static bool dbl_is_sorted_nonstrictly_decreasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_DECREASING_NA_LARGEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_nonstrictly_decreasing_na_largest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_DECREASING_NA_LARGEST(int, INTEGER_RO, int_is_na);
}

// -----------------------------------------------------------------------------

#define IS_SORTED_NONSTRICTLY_DECREASING_NA_SMALLEST(CTYPE, CONST_DEREF, IS_NA) { \
  const CTYPE* p_x = CONST_DEREF(x);                                              \
  CTYPE previous = p_x[0];                                                        \
  bool previous_is_na = IS_NA(previous);                                          \
                                                                                  \
  for (R_xlen_t i = 1; i < size; ++i) {                                           \
    const CTYPE current = p_x[i];                                                 \
    const bool current_is_na = IS_NA(current);                                    \
                                                                                  \
    if (previous_is_na) {                                                         \
      if (current_is_na) {                                                        \
        previous = current;                                                       \
        previous_is_na = current_is_na;                                           \
        continue;                                                                 \
      } else {                                                                    \
        return false;                                                             \
      }                                                                           \
    }                                                                             \
                                                                                  \
    if (previous < current) {                                                     \
      /* Catch the transition to NAs at the end */                                \
      if (current_is_na) {                                                        \
        previous = current;                                                       \
        previous_is_na = current_is_na;                                           \
        continue;                                                                 \
      }                                                                           \
                                                                                  \
      return false;                                                               \
    }                                                                             \
                                                                                  \
    previous = current;                                                           \
    previous_is_na = current_is_na;                                               \
  }                                                                               \
                                                                                  \
  return true;                                                                    \
}

static bool dbl_is_sorted_nonstrictly_decreasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_DECREASING_NA_SMALLEST(double, REAL_RO, dbl_is_na);
}

static bool int_is_sorted_nonstrictly_decreasing_na_smallest(SEXP x, R_xlen_t size) {
  IS_SORTED_NONSTRICTLY_DECREASING_NA_SMALLEST(int, INTEGER_RO, int_is_na);
}

// -----------------------------------------------------------------------------

static bool pull_strictly(SEXP strictly) {
  if (TYPEOF(strictly) != LGLSXP || Rf_length(strictly) != 1) {
    Rf_errorcall(R_NilValue, "`strictly` must be a single logical value.");
  }

  int out = LOGICAL(strictly)[0];

  if (out == NA_LOGICAL) {
    Rf_errorcall(R_NilValue, "`strictly` must not be `NA`.");
  }

  return (bool) out;
}

static bool pull_increasing(SEXP increasing) {
  if (TYPEOF(increasing) != LGLSXP || Rf_length(increasing) != 1) {
    Rf_errorcall(R_NilValue, "`increasing` must be a single logical value.");
  }

  int out = LOGICAL(increasing)[0];

  if (out == NA_LOGICAL) {
    Rf_errorcall(R_NilValue, "`increasing` must not be `NA`.");
  }

  return (bool) out;
}

static bool pull_na_largest(SEXP na_value) {
  if (TYPEOF(na_value) != STRSXP || Rf_length(na_value) != 1) {
    Rf_errorcall(R_NilValue, "`na_value` must be a single character value.");
  }

  const char* string = CHAR(STRING_ELT(na_value, 0));

  if (strcmp(string, "smallest") == 0) {
    return false;
  }

  if (strcmp(string, "largest") == 0) {
    return true;
  }

  Rf_errorcall(R_NilValue, "`na_value` must be either 'largest' or 'smallest'.");
}
