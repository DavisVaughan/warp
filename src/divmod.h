#ifndef WARP_DIVMOD_H
#define WARP_DIVMOD_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <float.h>

void divmod(int x, int y, int* p_quot, int* p_rem);
int int_div(int x, int y);

#endif
