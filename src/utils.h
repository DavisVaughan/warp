#ifndef WARP_UTILS_H
#define WARP_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdarg.h>

// -----------------------------------------------------------------------------

enum warp_period_type {
  warp_period_year,
  warp_period_quarter,
  warp_period_month,
  warp_period_week,
  warp_period_yweek,
  warp_period_day,
  warp_period_yday,
  warp_period_hour,
  warp_period_minute,
  warp_period_second,
  warp_period_millisecond
};

enum warp_period_type as_period_type(SEXP period);

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

bool str_equal(const char* x, const char* y);

SEXP as_posixct_from_posixlt(SEXP x);
SEXP as_posixlt_from_posixct(SEXP x);
SEXP as_date(SEXP x);

// In `get.c`
SEXP get_year_offset(SEXP x);
SEXP get_month_offset(SEXP x);
SEXP get_day_offset(SEXP x);
SEXP get_yweek_offset(SEXP x);

// In `date.c`
SEXP date_get_year_offset(SEXP x);
SEXP date_get_month_offset(SEXP x);
SEXP date_get_yweek_offset(SEXP x);

// In `coercion.c`
SEXP as_datetime(SEXP x);

SEXP get_origin_epoch_in_time_zone(SEXP x);
SEXP convert_time_zone(SEXP x, SEXP origin);

extern SEXP syms_tzone;
extern SEXP syms_class;

extern SEXP classes_data_frame;
extern SEXP classes_posixct;

extern SEXP strings_start_stop;

#endif
