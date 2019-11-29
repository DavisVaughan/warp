#include "utils.h"

enum timeslide_unique_type as_unique_type(int type) {
  switch (type) {
  case 1: return timeslide_unique_year;
  case 2: return timeslide_unique_month;
  case 3: return timeslide_unique_week;
  case 4: return timeslide_unique_day;
  case 5: return timeslide_unique_hour;
  case 6: return timeslide_unique_minute;
  case 7: return timeslide_unique_second;
  case 8: return timeslide_unique_millisecond;
  default: Rf_errorcall(R_NilValue, "Internal error: unknown `type`.");
  }
}
