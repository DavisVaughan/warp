#ifndef TIMEWARP_H
#define TIMEWARP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdbool.h>
#include "utils.h"

#define PROTECT_N(x, n) (++*n, PROTECT(x))

// Functionality ------------------------------------------------

SEXP warp_group(SEXP x, enum timewarp_group_type type, int every, SEXP origin);
SEXP warp_breaks(SEXP x, enum timewarp_group_type type, int every, SEXP origin);
SEXP warp_changes(SEXP x);
SEXP warp_ranges(SEXP x);

// Compatibility ------------------------------------------------

#if (R_VERSION < R_Version(3, 5, 0))
# define LOGICAL_RO(x) ((const int*) LOGICAL(x))
# define INTEGER_RO(x) ((const int*) INTEGER(x))
# define REAL_RO(x) ((const double*) REAL(x))
# define COMPLEX_RO(x) ((const Rcomplex*) COMPLEX(x))
# define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
# define RAW_RO(x) ((const Rbyte*) RAW(x))
#endif

#endif
