#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP warp_warp_distance(SEXP, SEXP, SEXP, SEXP);
extern SEXP warp_warp_change(SEXP, SEXP, SEXP, SEXP);
extern SEXP warp_warp_boundary(SEXP, SEXP, SEXP, SEXP);

extern SEXP warp_class_type(SEXP);
extern SEXP warp_date_get_year_offset(SEXP);
extern SEXP warp_date_get_month_offset(SEXP);
extern SEXP warp_convert_days_to_components(SEXP);
extern SEXP warp_divmod(SEXP, SEXP);
extern SEXP warp_div(SEXP, SEXP);
extern SEXP warp_warp_is_sorted(SEXP, SEXP, SEXP, SEXP);
extern SEXP warp_get_origin_epoch_in_time_zone(SEXP);

// Defined below
SEXP warp_init_library(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"warp_warp_distance",                 (DL_FUNC) &warp_warp_distance, 4},
  {"warp_warp_change",                   (DL_FUNC) &warp_warp_change, 4},
  {"warp_warp_boundary",                 (DL_FUNC) &warp_warp_boundary, 4},
  {"warp_class_type",                    (DL_FUNC) &warp_class_type, 1},
  {"warp_date_get_year_offset",          (DL_FUNC) &warp_date_get_year_offset, 1},
  {"warp_date_get_month_offset",         (DL_FUNC) &warp_date_get_month_offset, 1},
  {"warp_convert_days_to_components",    (DL_FUNC) &warp_convert_days_to_components, 1},
  {"warp_divmod",                        (DL_FUNC) &warp_divmod, 2},
  {"warp_div",                           (DL_FUNC) &warp_div, 2},
  {"warp_warp_is_sorted",                (DL_FUNC) &warp_warp_is_sorted, 4},
  {"warp_get_origin_epoch_in_time_zone", (DL_FUNC) &warp_get_origin_epoch_in_time_zone, 1},
  {"warp_init_library",                  (DL_FUNC) &warp_init_library, 1},
  {NULL, NULL, 0}
};

void R_init_warp(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

void warp_init_utils(SEXP ns);

SEXP warp_init_library(SEXP ns) {
  warp_init_utils(ns);
  return R_NilValue;
}
