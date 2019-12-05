#ifndef TIMEWARP_UTILS_H
#define TIMEWARP_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdarg.h>

enum timewarp_group_type {
  timewarp_group_year,
  timewarp_group_quarter,
  timewarp_group_month,
  timewarp_group_week,
  timewarp_group_day,
  timewarp_group_hour,
  timewarp_group_minute,
  timewarp_group_second,
  timewarp_group_millisecond
};

enum timewarp_class_type {
  timewarp_class_date,
  timewarp_class_posixct,
  timewarp_class_posixlt,
  timewarp_class_unknown
};

enum timewarp_class_type time_class_type(SEXP x);

enum timewarp_group_type as_group_type(SEXP by);

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
