#include "timeslide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

SEXP validate_origin(SEXP origin) {
  if (origin == R_NilValue) {
    return(origin);
  }

  R_len_t n_origin = Rf_length(origin);

  if (n_origin != 1) {
    Rf_errorcall(R_NilValue, "`origin` must have size 1, not %i.", n_origin);
  }

  if (time_class_type(origin) == timeslide_class_unknown) {
    Rf_errorcall(R_NilValue, "`origin` must inherit from 'Date', 'POSIXct', or 'POSIXlt'.");
  }

  SEXP out = PROTECT(as_datetime(origin));

  if (REAL(out)[0] == NA_REAL) {
    Rf_errorcall(R_NilValue, "`origin` must be a valid date value, not `NA`.");
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

SEXP warp_chunk(SEXP x, enum timeslide_chunk_type type, SEXP origin);

// [[ register() ]]
SEXP timeslide_warp_chunk(SEXP x, SEXP by, SEXP origin) {
  enum timeslide_chunk_type type = as_chunk_type(by);
  return warp_chunk(x, type, origin);
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_year(SEXP x, SEXP origin);
static SEXP warp_chunk_month(SEXP x, SEXP origin);
static SEXP warp_chunk_day(SEXP x, SEXP origin);

// [[ include("timeslide.h") ]]
SEXP warp_chunk(SEXP x, enum timeslide_chunk_type type, SEXP origin) {
  origin = PROTECT(validate_origin(origin));

  SEXP out;

  switch (type) {
  case timeslide_chunk_year: out = PROTECT(warp_chunk_year(x, origin)); break;
  case timeslide_chunk_month: out = PROTECT(warp_chunk_month(x, origin)); break;
  case timeslide_chunk_day: out = PROTECT(warp_chunk_day(x, origin)); break;
  default: Rf_errorcall(R_NilValue, "Internal error: unknown `type`.");
  }

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_year(SEXP x, SEXP origin) {
  int n_prot = 0;

  int origin_year = 1970;

  if (origin != R_NilValue) {
    SEXP origin_time_df = PROTECT_N(time_get(origin, strings_year), &n_prot);
    origin_year = INTEGER(VECTOR_ELT(origin_time_df, 0))[0];
  }

  SEXP time_df = PROTECT_N(time_get(x, strings_year), &n_prot);
  SEXP out = VECTOR_ELT(time_df, 0);

  out = PROTECT_N(r_maybe_duplicate(out), &n_prot);
  int* p_out = INTEGER(out);

  R_xlen_t n_out = Rf_xlength(out);

  for (R_xlen_t i = 0; i < n_out; ++i) {
    p_out[i] -= origin_year;
  }

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP warp_chunk_month(SEXP x, SEXP origin) {
  int n_prot = 0;

  int origin_year = 1970;
  int origin_month = 0;

  if (origin != R_NilValue) {
    SEXP origin_time_df = PROTECT_N(time_get(origin, strings_year_month), &n_prot);
    origin_year = INTEGER(VECTOR_ELT(origin_time_df, 0))[0];
    origin_month = INTEGER(VECTOR_ELT(origin_time_df, 1))[0] - 1;
  }

  SEXP time_df = PROTECT_N(time_get(x, strings_year_month), &n_prot);

  SEXP year = VECTOR_ELT(time_df, 0);
  SEXP month = VECTOR_ELT(time_df, 1);

  const int* p_year = INTEGER_RO(year);
  const int* p_month = INTEGER_RO(month);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, size), &n_prot);
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = (p_year[i] - origin_year) * 12 + (p_month[i] - 1 - origin_month);
  }

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP int_date_warp_chunk_day(SEXP x, SEXP origin) {
  SEXP out = PROTECT(r_maybe_duplicate(x));
  SET_ATTRIB(out, R_NilValue);
  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_chunk_day(SEXP x, SEXP origin) {
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

static SEXP date_warp_chunk_day(SEXP x, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_chunk_day(x, origin);
  case REALSXP: return dbl_date_warp_chunk_day(x, origin);
  default: Rf_errorcall(R_NilValue, "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

static SEXP int_posixct_warp_chunk_day(SEXP x, SEXP origin) {
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

static SEXP dbl_posixct_warp_chunk_day(SEXP x, SEXP origin) {
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

static SEXP posixct_warp_chunk_day(SEXP x, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_chunk_day(x, origin);
  case REALSXP: return dbl_posixct_warp_chunk_day(x, origin);
  default: Rf_errorcall(R_NilValue, "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}

static SEXP posixlt_warp_chunk_day(SEXP x, SEXP origin) {
  return x;
}

static SEXP warp_chunk_day(SEXP x, SEXP origin) {
  switch (time_class_type(x)) {
  case timeslide_class_date: return date_warp_chunk_day(x, origin);
  case timeslide_class_posixct: return posixct_warp_chunk_day(x, origin);
  case timeslide_class_posixlt: return posixlt_warp_chunk_day(x, origin);
  default: Rf_errorcall(R_NilValue, "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}
