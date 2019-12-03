#include "timeslide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

SEXP warp_chunk(SEXP x, enum timeslide_unique_type type);

// [[ register() ]]
SEXP timeslide_warp_chunk(SEXP x, SEXP type) {
  const int type_ = INTEGER_RO(type)[0];
  enum timeslide_unique_type unique_type = as_unique_type(type_);
  return warp_chunk(x, unique_type);
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_year(SEXP x);
static SEXP warp_chunk_month(SEXP x);
static SEXP warp_chunk_day(SEXP x);

// [[ include("timeslide.h") ]]
SEXP warp_chunk(SEXP x, enum timeslide_unique_type type) {
  switch (type) {
  case timeslide_unique_year: return warp_chunk_year(x);
  case timeslide_unique_month: return warp_chunk_month(x);
  case timeslide_unique_day: return warp_chunk_day(x);
  default: Rf_errorcall(R_NilValue, "Internal error: unknown `type`.");
  }
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_year(SEXP x) {
  SEXP time_df = PROTECT(time_get(x, strings_year));
  SEXP out = VECTOR_ELT(time_df, 0);

  out = PROTECT(r_maybe_duplicate(out));
  int* p_out = INTEGER(out);

  R_xlen_t n_out = Rf_xlength(out);

  for (R_xlen_t i = 0; i < n_out; ++i) {
    p_out[i] -= 1970;
  }

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_month(SEXP x) {
  SEXP time_df = PROTECT(time_get(x, strings_year_month));

  SEXP year = VECTOR_ELT(time_df, 0);
  SEXP month = VECTOR_ELT(time_df, 1);

  const int* p_year = INTEGER_RO(year);
  const int* p_month = INTEGER_RO(month);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = (p_year[i] - 1970) * 12 + p_month[i] - 1;
  }

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

#define IS_LEAP_YEAR(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define DAYS_IN_YEAR(year) (IS_LEAP_YEAR(year) ? 366 : 365)

static SEXP int_date_warp_chunk_day(SEXP x) {
  SEXP out = PROTECT(r_maybe_duplicate(x));
  SET_ATTRIB(out, R_NilValue);
  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_chunk_day(SEXP x) {
  R_xlen_t x_size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double elt = p_x[i];

    if (!R_FINITE(elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    if (elt < 0) {
      p_out[i] = (int) floor(elt);
    } else {
      p_out[i] = (int) elt;
    }
  }

  UNPROTECT(1);
  return out;
}

static SEXP date_warp_chunk_day(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_chunk_day(x);
  case REALSXP: return dbl_date_warp_chunk_day(x);
  default: Rf_errorcall(R_NilValue, "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

static SEXP int_posixct_warp_chunk_day(SEXP x) {
  R_xlen_t x_size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_size));
  int* p_out = INTEGER(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = elt / 86400;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_chunk_day(SEXP x) {
  R_xlen_t x_size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_size));
  int* p_out = INTEGER(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double elt = p_x[i];

    if (!R_FINITE(elt)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    if (elt < 0) {
      p_out[i] = (int) (floor(elt) / 86400);
    } else {
      p_out[i] = (int) (elt / 86400);
    }
  }

  UNPROTECT(1);
  return out;
}

static SEXP posixct_warp_chunk_day(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_chunk_day(x);
  case REALSXP: return dbl_posixct_warp_chunk_day(x);
  default: Rf_errorcall(R_NilValue, "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

static SEXP posixlt_warp_chunk_day(SEXP x) {
  return x;
}

static SEXP warp_chunk_day(SEXP x) {
  switch (time_class_type(x)) {
  case timeslide_class_date: return date_warp_chunk_day(x);
  case timeslide_class_posixct: return posixct_warp_chunk_day(x);
  case timeslide_class_posixlt: return posixlt_warp_chunk_day(x);
  default: Rf_errorcall(R_NilValue, "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}

#undef IS_LEAP_YEAR
#undef DAYS_IN_YEAR
