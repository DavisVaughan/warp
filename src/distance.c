#include "warp.h"
#include "utils.h"
#include <stdint.h> // For int64_t (especially on Windows)

// Helpers defined at the bottom of the file
static void validate_every(int every);
static void validate_origin(SEXP origin);
static double origin_to_days_from_epoch(SEXP origin);
static double origin_to_seconds_from_epoch(SEXP origin);
static inline double guard_with_microsecond(double x);

// -----------------------------------------------------------------------------

static SEXP warp_distance_year(SEXP x, int every, SEXP origin);
static SEXP warp_distance_quarter(SEXP x, int every, SEXP origin);
static SEXP warp_distance_month(SEXP x, int every, SEXP origin);
static SEXP warp_distance_week(SEXP x, int every, SEXP origin);
static SEXP warp_distance_day(SEXP x, int every, SEXP origin);
static SEXP warp_distance_hour(SEXP x, int every, SEXP origin);
static SEXP warp_distance_minute(SEXP x, int every, SEXP origin);
static SEXP warp_distance_second(SEXP x, int every, SEXP origin);
static SEXP warp_distance_millisecond(SEXP x, int every, SEXP origin);

// [[ include("warp.h") ]]
SEXP warp_distance(SEXP x, enum warp_by_type type, int every, SEXP origin) {
  validate_origin(origin);
  validate_every(every);

  if (time_class_type(x) == warp_class_unknown) {
    r_error("warp_distance", "`x` must inherit from 'Date', 'POSIXct', or 'POSIXlt'.");
  }

  const char* origin_timezone = get_timezone(origin);
  x = PROTECT(convert_timezone(x, origin_timezone));

  SEXP out;

  switch (type) {
  case warp_by_year: out = PROTECT(warp_distance_year(x, every, origin)); break;
  case warp_by_quarter: out = PROTECT(warp_distance_quarter(x, every, origin)); break;
  case warp_by_month: out = PROTECT(warp_distance_month(x, every, origin)); break;
  case warp_by_week: out = PROTECT(warp_distance_week(x, every, origin)); break;
  case warp_by_day: out = PROTECT(warp_distance_day(x, every, origin)); break;
  case warp_by_hour: out = PROTECT(warp_distance_hour(x, every, origin)); break;
  case warp_by_minute: out = PROTECT(warp_distance_minute(x, every, origin)); break;
  case warp_by_second: out = PROTECT(warp_distance_second(x, every, origin)); break;
  case warp_by_millisecond: out = PROTECT(warp_distance_millisecond(x, every, origin)); break;
  default: r_error("warp_distance", "Internal error: unknown `type`.");
  }

  UNPROTECT(2);
  return out;
}

// [[ register() ]]
SEXP warp_warp_distance(SEXP x, SEXP by, SEXP every, SEXP origin) {
  enum warp_by_type type = as_by_type(by);
  int every_ = pull_every(every);
  return warp_distance(x, type, every_, origin);
}

// -----------------------------------------------------------------------------

static SEXP warp_distance_year(SEXP x, int every, SEXP origin) {
  int n_prot = 0;

  bool needs_offset = (origin != R_NilValue);

  int origin_offset;

  if (needs_offset) {
    SEXP origin_offset_sexp = PROTECT_N(get_year_offset(origin), &n_prot);
    origin_offset = INTEGER(origin_offset_sexp)[0];

    if (origin_offset == NA_INTEGER) {
      r_error("warp_distance_year", "`origin` cannot be `NA`.");
    }
  }

  bool needs_every = (every != 1);

  x = PROTECT_N(get_year_offset(x), &n_prot);
  int* p_x = INTEGER(x);

  R_xlen_t n_out = Rf_xlength(x);

  SEXP out = PROTECT_N(Rf_allocVector(REALSXP, n_out), &n_prot);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n_out; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      p_out[i] = (elt - (every - 1)) / every;
    } else {
      p_out[i] = elt / every;
    }
  }

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP warp_distance_quarter(SEXP x, int every, SEXP origin) {
  return warp_distance_month(x, every * 3, origin);
}

// -----------------------------------------------------------------------------

#define MONTHS_IN_YEAR 12

static SEXP warp_distance_month(SEXP x, int every, SEXP origin) {
  int n_prot = 0;

  bool needs_offset = (origin != R_NilValue);

  int origin_offset_year;
  int origin_offset_month;

  if (needs_offset) {
    SEXP origin_offset_lst = PROTECT_N(get_year_month_offset(origin), &n_prot);
    origin_offset_year = INTEGER(VECTOR_ELT(origin_offset_lst, 0))[0];
    origin_offset_month = INTEGER(VECTOR_ELT(origin_offset_lst, 1))[0];

    if (origin_offset_year == NA_INTEGER) {
      r_error("warp_distance_month", "`origin` cannot be `NA`.");
    }
  }

  bool needs_every = (every != 1);

  SEXP offset_lst = PROTECT_N(get_year_month_offset(x), &n_prot);

  SEXP year = VECTOR_ELT(offset_lst, 0);
  SEXP month = VECTOR_ELT(offset_lst, 1);

  const int* p_year = INTEGER_RO(year);
  const int* p_month = INTEGER_RO(month);

  R_xlen_t size = Rf_xlength(year);

  SEXP out = PROTECT_N(Rf_allocVector(REALSXP, size), &n_prot);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt_year = p_year[i];
    int elt_month = p_month[i];

    if (elt_year == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    int elt;

    if (needs_offset) {
      elt = (elt_year - origin_offset_year) * MONTHS_IN_YEAR + (elt_month - origin_offset_month);
    } else {
      elt = elt_year * MONTHS_IN_YEAR + elt_month;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(n_prot);
  return out;
}

#undef EPOCH_YEAR
#undef EPOCH_MONTH
#undef MONTHS_IN_YEAR

// -----------------------------------------------------------------------------

static SEXP warp_distance_week(SEXP x, int every, SEXP origin) {
  return warp_distance_day(x, every * 7, origin);
}

// -----------------------------------------------------------------------------

static SEXP date_warp_distance_day(SEXP x, int every, SEXP origin);
static SEXP posixct_warp_distance_day(SEXP x, int every, SEXP origin);
static SEXP posixlt_warp_distance_day(SEXP x, int every, SEXP origin);

static SEXP warp_distance_day(SEXP x, int every, SEXP origin) {
  switch (time_class_type(x)) {
  case warp_class_date: return date_warp_distance_day(x, every, origin);
  case warp_class_posixct: return posixct_warp_distance_day(x, every, origin);
  case warp_class_posixlt: return posixlt_warp_distance_day(x, every, origin);
  default: r_error("warp_distance_day", "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_date_warp_distance_day(SEXP x, int every, SEXP origin);
static SEXP dbl_date_warp_distance_day(SEXP x, int every, SEXP origin);

static SEXP date_warp_distance_day(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_distance_day(x, every, origin);
  case REALSXP: return dbl_date_warp_distance_day(x, every, origin);
  default: r_error("date_warp_distance_day", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_posixct_warp_distance_day(SEXP x, int every, SEXP origin);
static SEXP dbl_posixct_warp_distance_day(SEXP x, int every, SEXP origin);

static SEXP posixct_warp_distance_day(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_distance_day(x, every, origin);
  case REALSXP: return dbl_posixct_warp_distance_day(x, every, origin);
  default: r_error("posixct_warp_distance_day", "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP posixlt_warp_distance_day(SEXP x, int every, SEXP origin) {
  x = PROTECT(as_datetime(x));
  SEXP out = PROTECT(posixct_warp_distance_day(x, every, origin));

  UNPROTECT(2);
  return out;
}


static SEXP int_date_warp_distance_day(SEXP x, int every, SEXP origin) {
  bool needs_every = (every != 1);
  bool needs_offset = (origin != R_NilValue);

  // Early exit if no changes are required, the raw `day` is enough
  if (!needs_every && !needs_offset) {
    SEXP out = PROTECT(Rf_coerceVector(x, REALSXP));
    SET_ATTRIB(out, R_NilValue);
    SET_OBJECT(out, 0);
    UNPROTECT(1);
    return out;
  }

  R_xlen_t size = Rf_xlength(x);

  int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  double origin_offset;
  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin);
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_distance_day(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin);
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Truncate towards 0 to get rid of any fractional date pieces.
    // We ignore them completely, you should just uses a POSIXct if you
    // need them
    int elt = x_elt;

    // `origin_offset` should be correct from `as_date()` in
    // `origin_to_days_from_epoch()`, even if it had fractional parts
    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#define SECONDS_IN_DAY 86400

static SEXP int_posixct_warp_distance_day(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      elt -= origin_offset;
    }

    // Integer division, then straight into `elt` with no cast needed
    if (elt < 0) {
      elt = (elt - (SECONDS_IN_DAY - 1)) / SECONDS_IN_DAY;
    } else {
      elt = elt / SECONDS_IN_DAY;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_distance_day(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      x_elt -= origin_offset;
    }

    int elt;

    // Double division, then integer cast into `elt`
    if (x_elt < 0) {
      elt = (floor(x_elt) - (SECONDS_IN_DAY - 1)) / SECONDS_IN_DAY;
    } else {
      elt = x_elt / SECONDS_IN_DAY;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef SECONDS_IN_DAY

// -----------------------------------------------------------------------------

static SEXP date_warp_distance_hour(SEXP x, int every, SEXP origin);
static SEXP posixct_warp_distance_hour(SEXP x, int every, SEXP origin);
static SEXP posixlt_warp_distance_hour(SEXP x, int every, SEXP origin);

static SEXP warp_distance_hour(SEXP x, int every, SEXP origin) {
  switch (time_class_type(x)) {
  case warp_class_date: return date_warp_distance_hour(x, every, origin);
  case warp_class_posixct: return posixct_warp_distance_hour(x, every, origin);
  case warp_class_posixlt: return posixlt_warp_distance_hour(x, every, origin);
  default: r_error("warp_distance_hour", "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_date_warp_distance_hour(SEXP x, int every, SEXP origin);
static SEXP dbl_date_warp_distance_hour(SEXP x, int every, SEXP origin);

static SEXP date_warp_distance_hour(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_distance_hour(x, every, origin);
  case REALSXP: return dbl_date_warp_distance_hour(x, every, origin);
  default: r_error("date_warp_distance_hour", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_posixct_warp_distance_hour(SEXP x, int every, SEXP origin);
static SEXP dbl_posixct_warp_distance_hour(SEXP x, int every, SEXP origin);

static SEXP posixct_warp_distance_hour(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_distance_hour(x, every, origin);
  case REALSXP: return dbl_posixct_warp_distance_hour(x, every, origin);
  default: r_error("posixct_warp_distance_hour", "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP posixlt_warp_distance_hour(SEXP x, int every, SEXP origin) {
  x = PROTECT(as_datetime(x));
  SEXP out = PROTECT(posixct_warp_distance_hour(x, every, origin));

  UNPROTECT(2);
  return out;
}


#define HOURS_IN_DAY 24

static SEXP int_date_warp_distance_hour(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * HOURS_IN_DAY;
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    elt = elt * HOURS_IN_DAY;

    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_distance_hour(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * HOURS_IN_DAY;
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Truncate towards 0 to get rid of any fractional date pieces.
    // We ignore them completely, you should just uses a POSIXct if you
    // need them
    int elt = x_elt;

    elt = elt * HOURS_IN_DAY;

    // `origin_offset` should be correct from `as_date()` in
    // `origin_to_days_from_epoch()`, even if it had fractional parts
    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef HOURS_IN_DAY

#define SECONDS_IN_HOUR 3600

static SEXP int_posixct_warp_distance_hour(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      elt -= origin_offset;
    }

    // Integer division, then straight into `elt` with no cast needed
    if (elt < 0) {
      elt = (elt - (SECONDS_IN_HOUR - 1)) / SECONDS_IN_HOUR;
    } else {
      elt = elt / SECONDS_IN_HOUR;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_distance_hour(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      x_elt -= origin_offset;
    }

    int elt;

    // Double division, then integer cast into `elt`
    if (x_elt < 0) {
      elt = (floor(x_elt) - (SECONDS_IN_HOUR - 1)) / SECONDS_IN_HOUR;
    } else {
      elt = x_elt / SECONDS_IN_HOUR;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef SECONDS_IN_HOUR

// -----------------------------------------------------------------------------

static SEXP date_warp_distance_minute(SEXP x, int every, SEXP origin);
static SEXP posixct_warp_distance_minute(SEXP x, int every, SEXP origin);
static SEXP posixlt_warp_distance_minute(SEXP x, int every, SEXP origin);

static SEXP warp_distance_minute(SEXP x, int every, SEXP origin) {
  switch (time_class_type(x)) {
  case warp_class_date: return date_warp_distance_minute(x, every, origin);
  case warp_class_posixct: return posixct_warp_distance_minute(x, every, origin);
  case warp_class_posixlt: return posixlt_warp_distance_minute(x, every, origin);
  default: r_error("warp_distance_minute", "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_date_warp_distance_minute(SEXP x, int every, SEXP origin);
static SEXP dbl_date_warp_distance_minute(SEXP x, int every, SEXP origin);

static SEXP date_warp_distance_minute(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_distance_minute(x, every, origin);
  case REALSXP: return dbl_date_warp_distance_minute(x, every, origin);
  default: r_error("date_warp_distance_minute", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_posixct_warp_distance_minute(SEXP x, int every, SEXP origin);
static SEXP dbl_posixct_warp_distance_minute(SEXP x, int every, SEXP origin);

static SEXP posixct_warp_distance_minute(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_distance_minute(x, every, origin);
  case REALSXP: return dbl_posixct_warp_distance_minute(x, every, origin);
  default: r_error("posixct_warp_distance_minute", "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP posixlt_warp_distance_minute(SEXP x, int every, SEXP origin) {
  x = PROTECT(as_datetime(x));
  SEXP out = PROTECT(posixct_warp_distance_minute(x, every, origin));

  UNPROTECT(2);
  return out;
}


#define MINUTES_IN_DAY 1440

static SEXP int_date_warp_distance_minute(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * MINUTES_IN_DAY;
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    elt = elt * MINUTES_IN_DAY;

    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_distance_minute(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * MINUTES_IN_DAY;
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Truncate towards 0 to get rid of any fractional date pieces.
    // We ignore them completely, you should just uses a POSIXct if you
    // need them
    int elt = x_elt;

    elt = elt * MINUTES_IN_DAY;

    // `origin_offset` should be correct from `as_date()` in
    // `origin_to_days_from_epoch()`, even if it had fractional parts
    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef MINUTES_IN_DAY

#define SECONDS_IN_MINUTE 60

static SEXP int_posixct_warp_distance_minute(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      elt -= origin_offset;
    }

    // Integer division, then straight into `elt` with no cast needed
    if (elt < 0) {
      elt = (elt - (SECONDS_IN_MINUTE - 1)) / SECONDS_IN_MINUTE;
    } else {
      elt = elt / SECONDS_IN_MINUTE;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_distance_minute(SEXP x, int every, SEXP origin) {
  R_xlen_t size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      x_elt -= origin_offset;
    }

    int elt;

    // Double division, then integer cast into `elt`
    if (x_elt < 0) {
      elt = (floor(x_elt) - (SECONDS_IN_MINUTE - 1)) / SECONDS_IN_MINUTE;
    } else {
      elt = x_elt / SECONDS_IN_MINUTE;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef SECONDS_IN_MINUTE

// -----------------------------------------------------------------------------

static SEXP date_warp_distance_second(SEXP x, int every, SEXP origin);
static SEXP posixct_warp_distance_second(SEXP x, int every, SEXP origin);
static SEXP posixlt_warp_distance_second(SEXP x, int every, SEXP origin);

static SEXP warp_distance_second(SEXP x, int every, SEXP origin) {
  switch (time_class_type(x)) {
  case warp_class_date: return date_warp_distance_second(x, every, origin);
  case warp_class_posixct: return posixct_warp_distance_second(x, every, origin);
  case warp_class_posixlt: return posixlt_warp_distance_second(x, every, origin);
  default: r_error("warp_distance_second", "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_date_warp_distance_second(SEXP x, int every, SEXP origin);
static SEXP dbl_date_warp_distance_second(SEXP x, int every, SEXP origin);

static SEXP date_warp_distance_second(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_distance_second(x, every, origin);
  case REALSXP: return dbl_date_warp_distance_second(x, every, origin);
  default: r_error("date_warp_distance_second", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_posixct_warp_distance_second(SEXP x, int every, SEXP origin);
static SEXP dbl_posixct_warp_distance_second(SEXP x, int every, SEXP origin);

static SEXP posixct_warp_distance_second(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_distance_second(x, every, origin);
  case REALSXP: return dbl_posixct_warp_distance_second(x, every, origin);
  default: r_error("posixct_warp_distance_second", "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP posixlt_warp_distance_second(SEXP x, int every, SEXP origin) {
  x = PROTECT(as_datetime(x));
  SEXP out = PROTECT(posixct_warp_distance_second(x, every, origin));

  UNPROTECT(2);
  return out;
}


#define SECONDS_IN_DAY 86400

static SEXP int_date_warp_distance_second(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * SECONDS_IN_DAY;
  }

  for (R_xlen_t i = 0; i < x_size; ++i) {
    int x_elt = p_x[i];

    if (x_elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Convert to int64_t here to hold `elt * SECONDS_IN_DAY`
    // Can't be double because we still need integer division later
    int64_t elt = x_elt * SECONDS_IN_DAY;

    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_distance_second(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin) * SECONDS_IN_DAY;
  }

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Truncate towards 0 to get rid of the fractional pieces
    int64_t elt = x_elt;

    elt = elt * SECONDS_IN_DAY;

    // `origin_offset` should be correct from `as_date()` in
    // `origin_to_days_from_epoch()`, even if it had fractional parts
    if (needs_offset) {
      elt -= origin_offset;
    }

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef SECONDS_IN_DAY

static SEXP int_posixct_warp_distance_second(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    // Starts as `int`, since that is what `x` is
    int x_elt = p_x[i];

    if (x_elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Convert to `double` in case `x_elt_dbl -= origin_offset` goes OOB
    double x_elt_dbl = x_elt;

    if (needs_offset) {
      x_elt_dbl -= origin_offset;
    }

    // Guard and floor in case `origin` had fractional components
    x_elt_dbl = guard_with_microsecond(x_elt_dbl);

    int64_t elt = floor(x_elt_dbl);

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_distance_second(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      x_elt -= origin_offset;
    }

    x_elt = guard_with_microsecond(x_elt);

    // Always floor() to get rid of fractional seconds, whether `x_elt` is
    // negative or positive. Need int64_t here because of the integer
    // division later. Flooring takes the fractional seconds into account,
    // which we want to do.
    int64_t elt = floor(x_elt);

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP date_warp_distance_millisecond(SEXP x, int every, SEXP origin);
static SEXP posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin);
static SEXP posixlt_warp_distance_millisecond(SEXP x, int every, SEXP origin);

static SEXP warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  switch (time_class_type(x)) {
  case warp_class_date: return date_warp_distance_millisecond(x, every, origin);
  case warp_class_posixct: return posixct_warp_distance_millisecond(x, every, origin);
  case warp_class_posixlt: return posixlt_warp_distance_millisecond(x, every, origin);
  default: r_error("warp_distance_millisecond", "Unknown object with type, %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_date_warp_distance_millisecond(SEXP x, int every, SEXP origin);
static SEXP dbl_date_warp_distance_millisecond(SEXP x, int every, SEXP origin);

static SEXP date_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_date_warp_distance_millisecond(x, every, origin);
  case REALSXP: return dbl_date_warp_distance_millisecond(x, every, origin);
  default: r_error("date_warp_distance_millisecond", "Unknown `Date` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP int_posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin);
static SEXP dbl_posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin);

static SEXP posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  switch (TYPEOF(x)) {
  case INTSXP: return int_posixct_warp_distance_millisecond(x, every, origin);
  case REALSXP: return dbl_posixct_warp_distance_millisecond(x, every, origin);
  default: r_error("posixct_warp_distance_millisecond", "Unknown `POSIXct` type %s.", Rf_type2char(TYPEOF(x)));
  }
}


static SEXP posixlt_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  x = PROTECT(as_datetime(x));
  SEXP out = PROTECT(posixct_warp_distance_millisecond(x, every, origin));

  UNPROTECT(2);
  return out;
}


#define MILLISECONDS_IN_DAY 86400000

static SEXP int_date_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin);
  }

  for (R_xlen_t i = 0; i < x_size; ++i) {
    int x_elt = p_x[i];

    if (x_elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Convert to int64_t here to avoid overflow and
    // hold `elt * MILLISECONDS_IN_DAY`
    // Can't be double because we still need integer division later
    int64_t elt = x_elt;

    if (needs_offset) {
      elt -= origin_offset;
    }

    elt = elt * MILLISECONDS_IN_DAY;

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_date_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  double* p_x = REAL(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_days_from_epoch(origin);
  }

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Truncate towards 0 to get rid of the fractional pieces
    int64_t elt = x_elt;

    // `origin_offset` should be correct from `as_date()` in
    // `origin_to_days_from_epoch()`, even if it had fractional parts
    if (needs_offset) {
      elt -= origin_offset;
    }

    elt = elt * MILLISECONDS_IN_DAY;

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef MILLISECONDS_IN_DAY

#define MILLISECONDS_IN_SECOND 1000

static SEXP int_posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    // Starts as `int`, since that is what `x` is
    int x_elt = p_x[i];

    if (x_elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    // Convert to `double` to guard against overflow in
    // `x_elt_dbl - origin_offset` and preserve offset fractional pieces
    double x_elt_dbl = x_elt;

    if (needs_offset) {
      x_elt_dbl -= origin_offset;
    }

    // Must guard in case the origin offset had fractional pieces
    x_elt_dbl = guard_with_microsecond(x_elt_dbl);

    x_elt_dbl = x_elt_dbl * MILLISECONDS_IN_SECOND;

    int64_t elt = floor(x_elt_dbl);

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_posixct_warp_distance_millisecond(SEXP x, int every, SEXP origin) {
  R_xlen_t x_size = Rf_xlength(x);

  bool needs_every = (every != 1);

  bool needs_offset = (origin != R_NilValue);
  double origin_offset;

  if (needs_offset) {
    origin_offset = origin_to_seconds_from_epoch(origin);
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, x_size));
  double* p_out = REAL(out);

  double* p_x = REAL(x);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    double x_elt = p_x[i];

    if (!R_FINITE(x_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (needs_offset) {
      x_elt -= origin_offset;
    }

    // Guard before flooring. Must guard before converting to millisecond
    // as well, otherwise it would put the guard in the wrong decimal place
    x_elt = guard_with_microsecond(x_elt);

    x_elt = x_elt * MILLISECONDS_IN_SECOND;

    // Always floor() to get rid of fractional pieces
    int64_t elt = floor(x_elt);

    if (!needs_every) {
      p_out[i] = elt;
      continue;
    }

    if (elt < 0) {
      elt = (elt - (every - 1)) / every;
    } else {
      elt = elt / every;
    }

    p_out[i] = elt;
  }

  UNPROTECT(1);
  return out;
}

#undef MILLISECONDS_IN_SECOND

// -----------------------------------------------------------------------------

static void validate_every(int every) {
  if (every == NA_INTEGER) {
    r_error("validate_every", "`every` must not be `NA`");
  }

  if (every <= 0) {
    r_error("validate_every", "`every` must be an integer greater than 0, not %i", every);
  }
}

static void validate_origin(SEXP origin) {
  if (origin == R_NilValue) {
    return;
  }

  R_len_t n_origin = Rf_length(origin);

  if (n_origin != 1) {
    r_error("validate_origin", "`origin` must have size 1, not %i.", n_origin);
  }

  if (time_class_type(origin) == warp_class_unknown) {
    r_error("validate_origin", "`origin` must inherit from 'Date', 'POSIXct', or 'POSIXlt'.");
  }
}

// This will always return a double with no fractional component
static double origin_to_days_from_epoch(SEXP origin) {
  origin = PROTECT(as_date(origin));

  double out = REAL(origin)[0];

  if (out == NA_REAL) {
    r_error("origin_to_days_from_epoch", "`origin` must not be `NA`.");
  }

  UNPROTECT(1);
  return out;
}

static double origin_to_seconds_from_epoch(SEXP origin) {
  origin = PROTECT(as_datetime(origin));

  double out = REAL(origin)[0];

  if (out == NA_REAL) {
    r_error("origin_to_seconds_from_epoch", "`origin` must not be `NA`.");
  }

  UNPROTECT(1);
  return out;
}


/*
 * `double` values are represented with 64 bits:
 * - 1 sign bit
 * - 11 exponent bits
 * - 52 significand bits
 *
 * The 52 significand bits are the ones that store the true value, this
 * corresponds to about 15 stable significand digits, with everything after
 * that being garbage.
 *
 * Internally doubles are represented with scientific notation to put them in
 * the exponent-significand representation. So the following date, which
 * is represented as a double, really looks like this in scientific notation:
 *
 * unclass(as.POSIXct("2011-05-01 17:55:23.123456"))
 * =
 * 1304286923.1234560013
 * =
 * 1.3042869231234560013e+09
 *                ^ 15th digit
 *
 * Because only 15 digits are stable, this is where we draw the line on
 * assuming that the user might have some valuable information stored here.
 * This corresponds to the place right before microseconds. Sure, we could use
 * a date that has less digits before the decimal to get more fractional
 * precision (see below) but most dates are in this form: 10 digits before
 * the decimal representing whole seconds, meaning 5 stable digits after it.
 *
 * The other part of the story is that not all floating point numbers can be
 * represented exactly in binary. For example:
 *
 * unclass(as.POSIXct("1969-12-31 23:59:59.998", "UTC"))
 * =
 * -0.002000000000002444267
 *
 * Because of this, `floor()` will give results that (to us) are incorrect if
 * we were to try and floor to milliseconds. We would first times by 1000 to
 * get milliseconds of `-2.000000000002444267`, and then `floor()` would give
 * us -3, not -2 which is the correct group.
 *
 * To get around this, we need to guard against this floating point error. The
 * best way I can come up with is to add a small value before flooring, which
 * would push us into the -1.9999999 range, which would floor correctly.
 *
 * I chose the value of 1 microsecond because that is generally where the 16th
 * digit falls for most dates (10 digits of whole seconds, 5 of stable
 * fractional seconds). This seems to work well for the millisecond grouping,
 * and we apply it to anywhere that uses seconds "just in case", but it is hard
 * to come up with tests for them.
 */
static inline double guard_with_microsecond(double x) {
  return x + 0.000001;
}

