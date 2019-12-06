#include "timewarp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// [[ include("timewarp.h") ]]
SEXP warp_boundaries(SEXP x, enum timewarp_group_type type, int every, SEXP origin) {
  SEXP groups = PROTECT(warp_group(x, type, every, origin));
  SEXP out = PROTECT(locate_boundaries(groups));
  UNPROTECT(2);
  return out;
}

// [[ register() ]]
SEXP timewarp_warp_boundaries(SEXP x, SEXP by, SEXP every, SEXP origin) {
  enum timewarp_group_type type = as_group_type(by);
  int every_ = pull_every(every);
  return warp_boundaries(x, type, every_, origin);
}
