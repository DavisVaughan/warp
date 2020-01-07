#ifndef WARP_DATE_H
#define WARP_DATE_H

/*
 * @member year
 *   The year offset. The number of years since 1970.
 * @member month
 *   The month. Mapped to the range of 0-11, where 0 is January.
 * @member day
 *   The day of month. Mapped to the range of 0-30.
 * @member yday
 *   The day of the year. Mapped to the range of 0-365.
 */
struct warp_components {
  int year;
  int month;
  int day;
  int yday;
};

struct warp_components convert_days_to_components(int n);

#endif
