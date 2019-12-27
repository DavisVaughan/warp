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

int pull_every(SEXP every);

void __attribute__((noreturn)) never_reached(const char* fn);
void __attribute__((noreturn)) r_error(const char* where, const char* why, ...);

SEXP r_maybe_duplicate(SEXP x);

SEXP as_posixct_from_posixlt(SEXP x);
SEXP as_posixlt_from_posixct(SEXP x);
SEXP as_date(SEXP x);

// In `get.c`
SEXP get_year_offset(SEXP x);
SEXP get_month_offset(SEXP x);
SEXP get_day_offset(SEXP x);
SEXP get_week_offset(SEXP x);

// In `date.c`
SEXP date_get_year_offset(SEXP x);
SEXP date_get_month_offset(SEXP x);
SEXP date_get_week_offset(SEXP x);

// In `coercion.c`
SEXP as_datetime(SEXP x);

const char* get_timezone(SEXP x);
SEXP convert_timezone(SEXP x, const char* timezone);

extern SEXP classes_data_frame;

extern SEXP strings_start_stop;

#endif
