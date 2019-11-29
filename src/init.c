#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP timeslide_init_timerip();

static const R_CallMethodDef CallEntries[] = {
  {"timeslide_init_timerip", (DL_FUNC) &timeslide_init_timerip, 0},
  {NULL, NULL, 0}
};

void R_init_timeslide(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
