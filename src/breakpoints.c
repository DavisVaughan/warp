#include "timeslide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

SEXP breakpoints(SEXP x);

// [[ register() ]]
SEXP timeslide_breakpoints(SEXP x) {
  return breakpoints(x);
}

// -----------------------------------------------------------------------------

static SEXP new_breakpoints_df(R_len_t size);

// [[ include("timeslide.h") ]]
SEXP breakpoints(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    Rf_errorcall(R_NilValue, "`x` must be an integer vector.");
  }

  R_xlen_t size = Rf_xlength(x);

  if (size == 0) {
    SEXP out = PROTECT(new_breakpoints_df(size));

    SET_VECTOR_ELT(out, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(out, 1, Rf_allocVector(INTSXP, size));

    UNPROTECT(1);
    return out;
  }

  if (size == 1) {
    SEXP out = PROTECT(new_breakpoints_df(size));

    SET_VECTOR_ELT(out, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(out, 1, Rf_allocVector(INTSXP, size));

    INTEGER(VECTOR_ELT(out, 0))[0] = 1;
    INTEGER(VECTOR_ELT(out, 1))[0] = 1;

    UNPROTECT(1);
    return out;
  }

  // Maximum size is if all values are unique
  SEXP stops = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_stops = INTEGER(stops);

  int* p_x = INTEGER(x);

  int count = 0;
  int pos_last = 0;

  for (R_xlen_t i = 1; i < size; ++i) {
    int previous = p_x[i - 1];
    int current = p_x[i];

    if (current == previous) {
      continue;
    }

    // R indexed
    p_stops[count] = i; // - 1 + 1

    count++;
    pos_last = i;
  }

  // Always include the last value
  if (pos_last != size) {
    p_stops[count] = size;
    count++;
  }

  stops = PROTECT(Rf_lengthgets(stops, count));

  SEXP starts = PROTECT(Rf_allocVector(INTSXP, count));
  int* p_starts = INTEGER(starts);

  p_starts[0] = 1;

  for (int i = 1; i < count; ++i) {
    p_starts[i] = p_stops[i - 1] + 1;
  }

  SEXP out = PROTECT(new_breakpoints_df(count));

  SET_VECTOR_ELT(out, 0, starts);
  SET_VECTOR_ELT(out, 1, stops);

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP new_row_name_info(R_len_t size) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));
  int* p_out = INTEGER(out);

  p_out[0] = NA_INTEGER;
  p_out[1] = -size;

  UNPROTECT(1);
  return out;
}

// TODO - Use pregenerated name strings
static SEXP new_breakpoints_df(R_len_t size) {
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
