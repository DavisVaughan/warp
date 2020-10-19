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
  warp_period_mweek,
  warp_period_day,
  warp_period_yday,
  warp_period_mday,
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

/*
 * @member year_offset
 *   The year offset. The number of years since 1970.
 * @member month
 *   The month. Mapped to the range of 0-11, where 0 is January.
 * @member day
 *   The day of month. Mapped to the range of 0-30.
 * @member yday
 *   The day of the year. Mapped to the range of 0-365.
 */
struct warp_components {
  int year_offset;
  int month;
  int day;
  int yday;
};

struct warp_components convert_days_to_components(int n);

// -----------------------------------------------------------------------------

struct warp_yday_components {
  int year_offset;
  int yday;
};

struct warp_mday_components {
  int year_offset;
  int month;
};

// In `get.c`
struct warp_yday_components get_origin_yday_components(SEXP origin);
struct warp_mday_components get_origin_mday_components(SEXP origin);

// In `date.c`
struct warp_yday_components date_get_origin_yday_components(SEXP origin);
struct warp_mday_components date_get_origin_mday_components(SEXP origin);

// -----------------------------------------------------------------------------

int pull_every(SEXP every);
bool pull_last(SEXP last);
bool pull_endpoint(SEXP endpoint);

void __attribute__((noreturn)) never_reached(const char* fn);
void __attribute__((noreturn)) r_error(const char* where, const char* why, ...);

SEXP r_maybe_duplicate(SEXP x);

bool str_equal(const char* x, const char* y);

int leap_years_before_and_including_year(int year_offset);

SEXP as_posixct_from_posixlt(SEXP x);
SEXP as_posixlt_from_posixct(SEXP x);
SEXP as_date(SEXP x);

// In `get.c`
SEXP get_year_offset(SEXP x);
SEXP get_month_offset(SEXP x);
SEXP get_day_offset(SEXP x);

// In `date.c`
SEXP date_get_year_offset(SEXP x);
SEXP date_get_month_offset(SEXP x);

// In `coercion.c`
SEXP as_datetime(SEXP x);

// In `timezone.c`
SEXP get_origin_epoch_in_time_zone(SEXP x);
SEXP convert_time_zone(SEXP x, SEXP origin);

extern SEXP syms_tzone;
extern SEXP syms_class;

extern SEXP classes_data_frame;
extern SEXP classes_posixct;

extern SEXP strings_start_stop;

#endif
