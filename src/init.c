#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP timeslide_warp_chunk(SEXP, SEXP);
extern SEXP timeslide_breakpoints(SEXP);
extern SEXP timeslide_class_type(SEXP);

// Defined below
SEXP timeslide_init_library(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"timeslide_uniquify",     (DL_FUNC) &timeslide_warp_chunk, 2},
  {"timeslide_breakpoints",  (DL_FUNC) &timeslide_breakpoints, 1},
  {"timeslide_class_type",   (DL_FUNC) &timeslide_class_type, 1},
  {"timeslide_init_library", (DL_FUNC) &timeslide_init_library, 1},
  {NULL, NULL, 0}
};

void R_init_timeslide(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

void timeslide_init_utils(SEXP ns);

SEXP timeslide_init_library(SEXP ns) {
  timeslide_init_utils(ns);
  return R_NilValue;
}
