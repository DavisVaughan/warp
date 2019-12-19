#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// [[ include("warp.h") ]]
SEXP warp_boundaries(SEXP x, enum warp_by_type type, int every, SEXP origin) {
  SEXP distances = PROTECT(warp_distance(x, type, every, origin));
  SEXP out = PROTECT(locate_boundaries(distances));
  UNPROTECT(2);
  return out;
}

// [[ register() ]]
SEXP warp_warp_boundaries(SEXP x, SEXP by, SEXP every, SEXP origin) {
  enum warp_by_type type = as_by_type(by);
  int every_ = pull_every(every);
  return warp_boundaries(x, type, every_, origin);
}
