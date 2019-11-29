#include <timerip.c>

SEXP timeslide_init_timerip() {
  timerip_init_api();
  return R_NilValue;
}
