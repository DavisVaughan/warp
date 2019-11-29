#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP timeslide_uniquify(SEXP, SEXP);
extern SEXP timeslide_breakpoints(SEXP);
extern SEXP timeslide_init_timerip();

static const R_CallMethodDef CallEntries[] = {
  {"timeslide_uniquify",     (DL_FUNC) &timeslide_uniquify, 2},
  {"timeslide_breakpoints",  (DL_FUNC) &timeslide_breakpoints, 1},
  {"timeslide_init_timerip", (DL_FUNC) &timeslide_init_timerip, 0},
  {NULL, NULL, 0}
};

void R_init_timeslide(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
