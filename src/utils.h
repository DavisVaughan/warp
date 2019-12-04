#ifndef TIMESLIDE_UTILS_H
#define TIMESLIDE_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdarg.h>

enum timeslide_group_type {
  timeslide_group_year,
  timeslide_group_month,
  timeslide_group_week,
  timeslide_group_day,
  timeslide_group_hour,
  timeslide_group_minute,
  timeslide_group_second,
  timeslide_group_millisecond
};

enum timeslide_class_type {
  timeslide_class_date,
  timeslide_class_posixct,
  timeslide_class_posixlt,
  timeslide_class_unknown
};

enum timeslide_class_type time_class_type(SEXP x);

enum timeslide_group_type as_group_type(SEXP by);

void __attribute__((noreturn)) r_error(const char* where, const char* why, ...);
SEXP r_maybe_duplicate(SEXP x);

SEXP time_get(SEXP x, SEXP components);
SEXP as_posixct_from_posixlt(SEXP x);
SEXP as_date(SEXP x);

const char* get_timezone(SEXP x);
SEXP convert_timezone(SEXP x, const char* timezone);

extern SEXP strings_year;
extern SEXP strings_year_month;

#endif
