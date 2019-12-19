#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP timewarp_warp_distance(SEXP, SEXP, SEXP, SEXP);
extern SEXP timewarp_warp_boundaries(SEXP, SEXP, SEXP, SEXP);
extern SEXP timewarp_locate_changes(SEXP);
extern SEXP timewarp_locate_boundaries(SEXP);
extern SEXP timewarp_class_type(SEXP);

// Defined below
SEXP timewarp_init_library(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"timewarp_warp_distance",     (DL_FUNC) &timewarp_warp_distance, 4},
  {"timewarp_warp_boundaries",   (DL_FUNC) &timewarp_warp_boundaries, 4},
  {"timewarp_locate_changes",    (DL_FUNC) &timewarp_locate_changes, 1},
  {"timewarp_locate_boundaries", (DL_FUNC) &timewarp_locate_boundaries, 1},
  {"timewarp_class_type",        (DL_FUNC) &timewarp_class_type, 1},
  {"timewarp_init_library",      (DL_FUNC) &timewarp_init_library, 1},
  {NULL, NULL, 0}
};

void R_init_timewarp(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

void timewarp_init_utils(SEXP ns);

SEXP timewarp_init_library(SEXP ns) {
  timewarp_init_utils(ns);
  return R_NilValue;
}
