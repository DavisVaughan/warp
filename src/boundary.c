#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP warp_boundary_impl(SEXP stops);

// [[ include("warp.h") ]]
SEXP warp_boundary(SEXP x, enum warp_period_type type, int every, SEXP origin) {
  static const bool last = true;
  static const bool endpoint = false;

  SEXP stops = PROTECT(warp_change(x, type, every, origin, last, endpoint));
  SEXP out = warp_boundary_impl(stops);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP warp_warp_boundary(SEXP x, SEXP period, SEXP every, SEXP origin) {
  enum warp_period_type type = as_period_type(period);
  int every_ = pull_every(every);
  return warp_boundary(x, type, every_, origin);
}

// -----------------------------------------------------------------------------

static SEXP new_boundary_df(R_len_t size);
static SEXP compute_starts(SEXP x, R_xlen_t size);

static SEXP warp_boundary_impl(SEXP stops) {
  R_xlen_t size = Rf_xlength(stops);

  SEXP out = PROTECT(new_boundary_df(size));

  SET_VECTOR_ELT(out, 0, compute_starts(stops, size));
  SET_VECTOR_ELT(out, 1, stops);

  UNPROTECT(1);
  return out;
}

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

static SEXP new_boundary_df(R_len_t size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  Rf_setAttrib(out, R_NamesSymbol, strings_start_stop);
  Rf_setAttrib(out, R_ClassSymbol, classes_data_frame);
  Rf_setAttrib(out, R_RowNamesSymbol, new_row_name_info(size));

  UNPROTECT(1);
  return out;
}
