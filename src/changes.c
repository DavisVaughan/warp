#include "timewarp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

SEXP warp_changes(SEXP x);
SEXP warp_ranges(SEXP x);

// [[ register() ]]
SEXP timewarp_warp_changes(SEXP x) {
  return warp_changes(x);
}

// [[ register() ]]
SEXP timewarp_warp_ranges(SEXP x) {
  return warp_ranges(x);
}

// -----------------------------------------------------------------------------

#define CHANGEPOINT_LOOP(CTYPE, CONST_DEREF) {                 \
  const CTYPE* p_x = CONST_DEREF(x);                           \
                                                               \
  CTYPE previous = p_x[0];                                     \
                                                               \
  for (R_xlen_t i = 1; i < size; ++i) {                        \
    const CTYPE current = p_x[i];                              \
                                                               \
    if (current == previous) {                                 \
      continue;                                                \
    }                                                          \
                                                               \
    /* R indexed, and really `- 1 + 1` */                      \
    p_out[count] = i;                                          \
                                                               \
    count++;                                                   \
    pos_last = i;                                              \
    previous = current;                                        \
  }                                                            \
}

// [[ include("timewarp.h") ]]
SEXP warp_changes(SEXP x) {
  SEXPTYPE type = TYPEOF(x);

  if (type != INTSXP && type != REALSXP) {
    r_error("warp_changes", "`x` must be an integer or double vector.");
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
  case INTSXP: CHANGEPOINT_LOOP(int, INTEGER_RO); break;
  case REALSXP: CHANGEPOINT_LOOP(double, REAL_RO); break;
  default: r_error("warp_changes", "Internal error: should have caught this earlier.");
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

// -----------------------------------------------------------------------------

static SEXP new_ranges_df(R_len_t size);
static SEXP compute_starts(SEXP x, R_xlen_t size);

// [[ include("timewarp.h") ]]
SEXP warp_ranges(SEXP x) {
  SEXP stops = PROTECT(warp_changes(x));

  R_xlen_t size = Rf_xlength(stops);

  SEXP out = PROTECT(new_ranges_df(size));

  SET_VECTOR_ELT(out, 0, compute_starts(stops, size));
  SET_VECTOR_ELT(out, 1, stops);

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP compute_starts(SEXP x, R_xlen_t size) {
  if (size == 0) {
    return Rf_allocVector(REALSXP, 0);
  }

  if (size == 1) {
    return Rf_ScalarReal(1);
  }

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  p_out[0] = 1;

  for (R_xlen_t i = 1; i < size; ++i) {
    p_out[i] = p_x[i - 1] + 1;
  }

  UNPROTECT(1);
  return out;
}

static SEXP new_row_name_info(R_len_t size) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));
  int* p_out = INTEGER(out);

  p_out[0] = NA_INTEGER;
  p_out[1] = -size;

  UNPROTECT(1);
  return out;
}

// TODO - Use pregenerated name strings
static SEXP new_ranges_df(R_len_t size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("start"));
  SET_STRING_ELT(names, 1, Rf_mkChar("stop"));

  SEXP classes_data_frame = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(classes_data_frame, 0, Rf_mkChar("data.frame"));

  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_ClassSymbol, classes_data_frame);
  Rf_setAttrib(out, R_RowNamesSymbol, new_row_name_info(size));

  UNPROTECT(3);
  return out;
}
