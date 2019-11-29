#include "timeslide.h"
#include "utils.h"
#include <timerip.h>

// -----------------------------------------------------------------------------

SEXP uniquify(SEXP x, enum timeslide_unique_type type);

// [[ register() ]]
SEXP timeslide_uniquify(SEXP x, SEXP type) {
  const int type_ = INTEGER_RO(type)[0];
  enum timeslide_unique_type unique_type = as_unique_type(type_);
  return uniquify(x, unique_type);
}

// -----------------------------------------------------------------------------

static SEXP uniquify_year(SEXP x);
static SEXP uniquify_month(SEXP x);

// [[ include("timeslide.h") ]]
SEXP uniquify(SEXP x, enum timeslide_unique_type type) {
  switch (type) {
  case timeslide_unique_year: return uniquify_year(x);
  case timeslide_unique_month: return uniquify_month(x);
  default: Rf_errorcall(R_NilValue, "Internal error: unknown `type`.");
  }
}

// -----------------------------------------------------------------------------

static SEXP uniquify_year(SEXP x) {
  return rip_year(x);
}

static SEXP uniquify_month(SEXP x) {
  SEXP year = PROTECT(rip_year(x));
  const int* p_year = INTEGER_RO(year);

  SEXP month = PROTECT(rip_month(x));
  const int* p_month = INTEGER_RO(month);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = p_year[i] * 100 + p_month[i];
  }

  UNPROTECT(3);
  return out;
}
