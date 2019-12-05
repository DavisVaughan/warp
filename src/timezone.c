#include "timewarp.h"
#include "utils.h"

// [[ include("utils.h") ]]
const char* get_timezone(SEXP x) {
  // Generally a signal for using unix epoch
  if (x == R_NilValue) {
    return "UTC";
  }

  enum timewarp_class_type type = time_class_type(x);

  if (type == timewarp_class_date) {
    return "UTC";
  }

  if (type == timewarp_class_unknown) {
    Rf_errorcall(R_NilValue, "Internal error: `x` has unknown date time class.");
  }

  SEXP tzone_attribute = PROTECT(Rf_getAttrib(x, Rf_install("tzone")));

  // Local time
  if (tzone_attribute == R_NilValue) {
    UNPROTECT(1);
    return "";
  }

  if (TYPEOF(tzone_attribute) != STRSXP) {
    Rf_errorcall(R_NilValue, "`tzone` attribute must be a character vector, or `NULL`.");
  }

  const char* timezone = CHAR(STRING_ELT(tzone_attribute, 0));

  UNPROTECT(1);
  return timezone;
}

const char* get_printable_timezone(const char* timezone) {
  if (strlen(timezone) == 0) {
    return "local";
  }

  return timezone;
}

// Converts `x` to a POSIXct with the correct timezone if required
// - Does nothing if `x` is a Date and timezone is UTC
// - Converts Dates to POSIXct if local time is requested

// [[ include("utils.h") ]]
SEXP convert_timezone(SEXP x, const char* timezone) {
  const char* x_timezone = get_timezone(x);

  if (strcmp(x_timezone, timezone) == 0) {
    return(x);
  }

  Rf_warningcall(
    R_NilValue,
    "`x` (%s) and `origin` (%s) do not have the same time zone. "
    "Converting `x` to the time zone of `origin`. "
    "It is highly advised to provide `x` and `origin` with the same time zone.",
    get_printable_timezone(x_timezone),
    get_printable_timezone(timezone)
  );

  SEXP out = PROTECT(as_datetime(x));
  out = PROTECT(r_maybe_duplicate(out));

  // Set to NULL for local time
  if (strlen(timezone) == 0) {
    Rf_setAttrib(out, Rf_install("tzone"), R_NilValue);
    UNPROTECT(2);
    return(out);
  }

  SEXP strings_tzone = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(strings_tzone, 0, Rf_mkChar(timezone));

  Rf_setAttrib(out, Rf_install("tzone"), strings_tzone);

  UNPROTECT(3);
  return out;
}
