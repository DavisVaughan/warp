#ifndef WARP_UTILS_H
#define WARP_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdarg.h>

// -----------------------------------------------------------------------------

enum warp_by_type {
  warp_by_year,
  warp_by_quarter,
  warp_by_month,
  warp_by_week,
  warp_by_day,
  warp_by_hour,
  warp_by_minute,
  warp_by_second,
  warp_by_millisecond
};

enum warp_by_type as_by_type(SEXP by);

// -----------------------------------------------------------------------------

enum warp_class_type {
  warp_class_date,
  warp_class_posixct,
  warp_class_posixlt,
  warp_class_unknown
};

enum warp_class_type time_class_type(SEXP x);

// -----------------------------------------------------------------------------
// Missing values

// Pulled from vctrs

// Annex F of C99 specifies that `double` should conform to the IEEE 754
// type `binary64`, which is defined as:
// * 1  bit : sign
// * 11 bits: exponent
// * 52 bits: significand
//
// R stores the value "1954" in the last 32 bits: this payload marks
// the value as a NA, not a regular NaN.
//
// On big endian systems, this corresponds to the second element of an
// integer array of size 2. On little endian systems, this is flipped
// and the NA marker is in the first element.
//
// The type assumptions made here are asserted in `vctrs_init_utils()`

#ifdef WORDS_BIGENDIAN
static const int warp_indicator_pos = 1;
#else
static const int warp_indicator_pos = 0;
#endif

union warp_dbl_indicator {
  double value;        // 8 bytes
  unsigned int key[2]; // 4 * 2 bytes
};

enum warp_dbl_class {
  warp_dbl_number,
  warp_dbl_missing,
  warp_dbl_nan
};

enum warp_dbl_class dbl_classify(double x);

// -----------------------------------------------------------------------------

int pull_every(SEXP every);

void __attribute__((noreturn)) never_reached(const char* fn);
void __attribute__((noreturn)) r_error(const char* where, const char* why, ...);

SEXP r_maybe_duplicate(SEXP x);

SEXP time_get(SEXP x, SEXP components);
SEXP as_posixct_from_posixlt(SEXP x);
SEXP as_date(SEXP x);

// In `coercion.c`
SEXP as_datetime(SEXP x);

const char* get_timezone(SEXP x);
SEXP convert_timezone(SEXP x, const char* timezone);

extern SEXP strings_year;
extern SEXP strings_year_month;

#endif
