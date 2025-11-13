#ifndef WARP_H
#define WARP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdbool.h>
#include "utils.h"

#define PROTECT_N(x, n) (++*n, PROTECT(x))

// Functionality ------------------------------------------------

SEXP warp_distance(SEXP x, enum warp_period_type type, int every, SEXP origin);

SEXP warp_change(SEXP x,
                 enum warp_period_type period,
                 int every,
                 SEXP origin,
                 bool last,
                 bool endpoint);

SEXP warp_boundary(SEXP x, enum warp_period_type type, int every, SEXP origin);

// Compatibility ------------------------------------------------

#endif
