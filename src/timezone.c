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

static SEXP make_tzone(const char* time_zone);

SEXP get_origin_epoch_in_time_zone(SEXP x) {
  const char* time_zone = get_time_zone(x);

  // Continue using `NULL` if `x` is UTC, no origin adjustment required
  if (str_equal(time_zone, "UTC") || str_equal(time_zone, "GMT")) {
    return R_NilValue;
  }

  SEXP dummy = PROTECT(Rf_ScalarReal(0));

  Rf_setAttrib(dummy, syms_tzone, make_tzone(time_zone));
  Rf_setAttrib(dummy, syms_class, classes_posixct);

  dummy = PROTECT(as_posixlt_from_posixct(dummy));

  // gmtoff is optional, it may not be there. In those cases assume UTC.
  if (Rf_length(dummy) != 11) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP offset_sexp = VECTOR_ELT(dummy, 10);
  int offset = INTEGER(offset_sexp)[0];

  // Documented as "unknown", assume UTC.
  if (offset == NA_INTEGER || offset == 0) {
    UNPROTECT(2);
    return R_NilValue;
  }

  double epoch_seconds = offset * -1.0;

  SEXP out = PROTECT(Rf_ScalarReal(epoch_seconds));

  Rf_setAttrib(out, syms_tzone, make_tzone(time_zone));
  Rf_setAttrib(out, syms_class, classes_posixct);

  UNPROTECT(3);
  return out;
}

static SEXP make_tzone(const char* time_zone) {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));

  SET_STRING_ELT(out, 0, Rf_mkChar(time_zone));

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// Converts `x` to a POSIXct with the correct time zone if required
// - Does nothing if `x` is a Date and time_zone is UTC
// - Converts Dates to POSIXct if local time is requested

// [[ include("utils.h") ]]
SEXP convert_time_zone(SEXP x, SEXP origin) {
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
