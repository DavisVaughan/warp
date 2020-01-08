#include "warp.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static const char* datetime_get_time_zone(SEXP x);

const char* get_time_zone(SEXP x) {
  switch(time_class_type(x)) {
  case warp_class_date: return "UTC";
  case warp_class_posixct:
  case warp_class_posixlt: return datetime_get_time_zone(x);
  default: r_error("get_time_zone", "Internal error: Unknown date time class.");
  }
}

static const char* datetime_get_time_zone(SEXP x) {
  SEXP tzone = Rf_getAttrib(x, syms_tzone);

  // Local time
  if (tzone == R_NilValue) {
    return "";
  }

  if (TYPEOF(tzone) != STRSXP) {
    r_error(
      "datetime_get_time_zone",
      "`tzone` attribute must be a character vector, or `NULL`."
    );
  }

  // Always grab the first element of the character vector.
  // For POSIXct it should always be length 1
  // For POSIXlt it can be length 1 (UTC) or length 3 (America/New_York, EST, EDT)
  // but we always want the first value.
  const char* time_zone = CHAR(STRING_ELT(tzone, 0));

  return time_zone;
}

const char* get_printable_time_zone(const char* time_zone) {
  if (strlen(time_zone) == 0) {
    return "local";
  }

  return time_zone;
}

// -----------------------------------------------------------------------------

// Converts `x` to a POSIXct with the correct time zone if required
// - Does nothing if `x` is a Date and time_zone is UTC
// - Converts Dates to POSIXct if local time is requested

// [[ include("utils.h") ]]
SEXP maybe_convert_time_zone(SEXP x, SEXP origin) {
  const char* x_time_zone = get_time_zone(x);
  const char* origin_time_zone = get_time_zone(origin);

  if (str_equal(x_time_zone, origin_time_zone)) {
    return(x);
  }

  Rf_warningcall(
    R_NilValue,
    "`x` (%s) and `origin` (%s) do not have the same time zone. "
    "Converting `x` to the time zone of `origin`. "
    "It is highly advised to provide `x` and `origin` with the same time zone.",
    get_printable_time_zone(x_time_zone),
    get_printable_time_zone(origin_time_zone)
  );

  SEXP out = PROTECT(as_datetime(x));
  out = PROTECT(r_maybe_duplicate(out));

  // Set to NULL for local time
  if (strlen(origin_time_zone) == 0) {
    Rf_setAttrib(out, syms_tzone, R_NilValue);
    UNPROTECT(2);
    return(out);
  }

  SEXP strings_tzone = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(strings_tzone, 0, Rf_mkChar(origin_time_zone));

  Rf_setAttrib(out, syms_tzone, strings_tzone);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

// Force to UTC, keeping the appearance of the clock time.
// This is useful for time zones that have DST when doing sub-daily
// grouping. For example, with:
//
// "1970-04-26 00:00:00 EST"
// "1970-04-26 01:00:00 EST"
// "1970-04-26 03:00:00 EDT"
// "1970-04-26 04:00:00 EDT"
//
// period = "hour", every = 2
//
// We want (0-1), (2-3), (4-5) to be grouped together, even though the 2 hour
// doesn't exist because of DST. If we grouped (0-1), (3-4) together then when
// it moved to the next day we'd end up with strange groups of (12-0), (1-2).
// Converting to UTC essentially materializes that 2nd hour and shifts the
// underlying numeric representation appropriately.

// [[ include("utils.h") ]]
SEXP maybe_force_utc_if_subdaily(SEXP x, enum warp_period_type type) {
  if (type != warp_period_hour &&
      type != warp_period_minute &&
      type != warp_period_second &&
      type != warp_period_millisecond) {
    return x;
  }

  if (time_class_type(x) == warp_class_date) {
    return x;
  }

  const char* time_zone = get_time_zone(x);

  if (str_equal(time_zone, "UTC") || str_equal(time_zone, "GMT")) {
    return x;
  }

  return force_utc(x);
}
