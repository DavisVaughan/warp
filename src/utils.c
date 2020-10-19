#include "utils.h"
#include "divmod.h"

// -----------------------------------------------------------------------------

SEXP warp_ns_env = NULL;

SEXP syms_x = NULL;
SEXP syms_tzone = NULL;
SEXP syms_class = NULL;

SEXP syms_as_posixct_from_posixlt = NULL;
SEXP syms_as_posixlt_from_posixct = NULL;
SEXP syms_as_date = NULL;

SEXP fns_as_posixct_from_posixlt = NULL;
SEXP fns_as_posixlt_from_posixct = NULL;
SEXP fns_as_date = NULL;

SEXP classes_data_frame = NULL;
SEXP classes_posixct = NULL;

SEXP strings_start_stop = NULL;

SEXP chars = NULL;
SEXP char_posixlt = NULL;
SEXP char_posixct = NULL;
SEXP char_posixt = NULL;
SEXP char_date = NULL;

// -----------------------------------------------------------------------------

enum warp_class_type time_class_type(SEXP x);
static enum warp_class_type time_class_type_impl(SEXP klass);
static const char* class_type_as_str(enum warp_class_type type);

// [[ register() ]]
SEXP warp_class_type(SEXP x) {
  return Rf_mkString(class_type_as_str(time_class_type(x)));
}

enum warp_class_type time_class_type(SEXP x) {
  if (!OBJECT(x)) {
    return warp_class_unknown;
  }

  SEXP klass = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  enum warp_class_type type = time_class_type_impl(klass);

  UNPROTECT(1);
  return type;
}

static enum warp_class_type time_class_type_impl(SEXP klass) {
  int n = Rf_length(klass);
  SEXP const* p_klass = STRING_PTR(klass);

  p_klass += n - 2;
  SEXP butlast = *p_klass++;
  SEXP last = *p_klass++;

  if (last == char_date) {
    return warp_class_date;
  }

  if (last == char_posixt) {
    if (butlast == char_posixlt) {
      return warp_class_posixlt;
    } else if (butlast == char_posixct) {
      return warp_class_posixct;
    }
  }

  return warp_class_unknown;
}

static const char* class_type_as_str(enum warp_class_type type) {
  switch (type) {
  case warp_class_date: return "date";
  case warp_class_posixct: return "posixct";
  case warp_class_posixlt: return "posixlt";
  case warp_class_unknown: return "unknown";
  }
  never_reached("class_type_as_str");
}

// -----------------------------------------------------------------------------

// TODO - Could be lossy...really should use vctrs? Callable from C?
int pull_every(SEXP every) {
  if (Rf_length(every) != 1) {
    r_error("pull_every", "`every` must have size 1, not %i", Rf_length(every));
  }

  if (OBJECT(every) != 0) {
    r_error("pull_every", "`every` must be a bare integer-ish value.");
  }

  switch (TYPEOF(every)) {
  case INTSXP: return INTEGER(every)[0];
  case REALSXP: return Rf_asInteger(every);
  default: r_error("pull_every", "`every` must be integer-ish, not %s", Rf_type2char(TYPEOF(every)));
  }
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
bool pull_endpoint(SEXP endpoint) {
  if (Rf_length(endpoint) != 1) {
    r_error("pull_endpoint", "`endpoint` must have size 1, not %i", Rf_length(endpoint));
  }

  if (OBJECT(endpoint) != 0) {
    r_error("pull_endpoint", "`endpoint` must be a bare logical value.");
  }

  switch (TYPEOF(endpoint)) {
  case LGLSXP: return LOGICAL(endpoint)[0];
  default: r_error("pull_endpoint", "`endpoint` must be logical, not %s", Rf_type2char(TYPEOF(endpoint)));
  }
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
bool pull_last(SEXP last) {
  if (Rf_length(last) != 1) {
    r_error("pull_last", "`last` must have size 1, not %i", Rf_length(last));
  }

  if (OBJECT(last) != 0) {
    r_error("pull_last", "`last` must be a bare logical value.");
  }

  switch (TYPEOF(last)) {
  case LGLSXP: return LOGICAL(last)[0];
  default: r_error("pull_last", "`last` must be logical, not %s", Rf_type2char(TYPEOF(last)));
  }
}

// -----------------------------------------------------------------------------

#define YEARS_FROM_0001_01_01_TO_EPOCH 1969
#define LEAP_YEARS_FROM_0001_01_01_TO_EPOCH 477

int leap_years_before_and_including_year(int year_offset) {
  int year = year_offset + YEARS_FROM_0001_01_01_TO_EPOCH;

  int n_leap_years =
    int_div(year, 4) -
    int_div(year, 100) +
    int_div(year, 400);

  n_leap_years -= LEAP_YEARS_FROM_0001_01_01_TO_EPOCH;

  return n_leap_years;
}

#undef YEARS_FROM_0001_01_01_TO_EPOCH
#undef LEAP_YEARS_FROM_0001_01_01_TO_EPOCH

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
bool str_equal(const char* x, const char* y) {
  return strcmp(x, y) == 0;
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
enum warp_period_type as_period_type(SEXP period) {
  if (TYPEOF(period) != STRSXP || Rf_length(period) != 1) {
    Rf_errorcall(R_NilValue, "`period` must be a single string.");
  }

  const char* type = CHAR(STRING_ELT(period, 0));

  if (str_equal(type, "year")) {
    return warp_period_year;
  }

  if (str_equal(type, "quarter")) {
    return warp_period_quarter;
  }

  if (str_equal(type, "month")) {
    return warp_period_month;
  }

  if (str_equal(type, "week")) {
    return warp_period_week;
  }

  if (str_equal(type, "yweek")) {
    return warp_period_yweek;
  }

  if (str_equal(type, "mweek")) {
    return warp_period_mweek;
  }

  if (str_equal(type, "day")) {
    return warp_period_day;
  }

  if (str_equal(type, "yday")) {
    return warp_period_yday;
  }

  if (str_equal(type, "mday")) {
    return warp_period_mday;
  }

  if (str_equal(type, "hour")) {
    return warp_period_hour;
  }

  if (str_equal(type, "minute")) {
    return warp_period_minute;
  }

  if (str_equal(type, "second")) {
    return warp_period_second;
  }

  if (str_equal(type, "millisecond")) {
    return warp_period_millisecond;
  }

  Rf_errorcall(R_NilValue, "Unknown `period` value '%s'.", type);
}

// -----------------------------------------------------------------------------

#define BUFSIZE 8192

// [[ include("utils.h") ]]
void __attribute__((noreturn)) r_error(const char* where, const char* why, ...) {
  char buf[BUFSIZE];

  va_list dots;
  va_start(dots, why);
  vsnprintf(buf, BUFSIZE, why, dots);
  va_end(dots);

  buf[BUFSIZE - 1] = '\0';

  Rf_errorcall(R_NilValue, "In C function `%s()`: %s", where, buf);
}

#undef BUFSIZE

// [[ include("utils.h") ]]
void __attribute__((noreturn)) never_reached(const char* fn) {
  r_error("never_reached", "Internal error in `%s()`: Reached the unreachable.", fn);
}

// -----------------------------------------------------------------------------

static SEXP r_env_get(SEXP env, SEXP sym) {
  SEXP obj = PROTECT(Rf_findVarInFrame3(env, sym, FALSE));

  // Force lazy loaded bindings
  if (TYPEOF(obj) == PROMSXP) {
    obj = Rf_eval(obj, R_BaseEnv);
  }

  UNPROTECT(1);
  return obj;
}

// [[ include("utils.h") ]]
SEXP r_maybe_duplicate(SEXP x) {
  if (MAYBE_REFERENCED(x)) {
    return Rf_shallow_duplicate(x);
  } else {
    return x;
  }
}

// -----------------------------------------------------------------------------

#include <R_ext/Parse.h>

static void abort_parse(SEXP code, const char* why) {
  if (Rf_GetOption1(Rf_install("rlang__verbose_errors")) != R_NilValue) {
    Rf_PrintValue(code);
  }
  Rf_error("Internal error: %s", why);
}

static SEXP r_parse(const char* str) {
  SEXP str_ = PROTECT(Rf_mkString(str));

  ParseStatus status;
  SEXP out = PROTECT(R_ParseVector(str_, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    abort_parse(str_, "Parsing failed");
  }
  if (Rf_length(out) != 1) {
    abort_parse(str_, "Expected a single expression");
  }

  out = VECTOR_ELT(out, 0);

  UNPROTECT(2);
  return out;
}

static SEXP r_parse_eval(const char* str, SEXP env) {
  SEXP out = Rf_eval(PROTECT(r_parse(str)), env);
  UNPROTECT(1);
  return out;
}

static SEXP new_env_call = NULL;
static SEXP new_env__parent_node = NULL;
static SEXP new_env__size_node = NULL;

static SEXP r_new_environment(SEXP parent, R_len_t size) {
  parent = parent ? parent : R_EmptyEnv;
  SETCAR(new_env__parent_node, parent);

  size = size ? size : 29;
  SETCAR(new_env__size_node, Rf_ScalarInteger(size));

  SEXP env = Rf_eval(new_env_call, R_BaseEnv);

  // Free for gc
  SETCAR(new_env__parent_node, R_NilValue);

  return env;
}

// -----------------------------------------------------------------------------

/**
 * Create a call or pairlist
 *
 * @param tags Optional. If not `NULL`, a null-terminated array of symbols.
 * @param cars Mandatory. A null-terminated array of CAR values.
 * @param fn The first CAR value of the language list.
 *
 */
static SEXP r_pairlist(SEXP* tags, SEXP* cars) {
  if (!cars) {
    Rf_error("Internal error: Null `cars` in `r_pairlist()`");
  }

  SEXP list = PROTECT(Rf_cons(R_NilValue, R_NilValue));
  SEXP node = list;

  while (*cars) {
    SEXP next_node = Rf_cons(*cars, R_NilValue);
    SETCDR(node, next_node);
    node = next_node;

    if (tags) {
      SET_TAG(next_node, *tags);
      ++tags;
    }

    ++cars;
  }

  UNPROTECT(1);
  return CDR(list);
}

static SEXP r_call(SEXP fn, SEXP* tags, SEXP* cars) {
  return Rf_lcons(fn, r_pairlist(tags, cars));
}

static SEXP warp_eval_mask_n_impl(SEXP fn, SEXP* syms, SEXP* args, SEXP mask) {
  SEXP call = PROTECT(r_call(fn, syms, syms));

  while (*syms) {
    Rf_defineVar(*syms, *args, mask);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(call, mask);

  UNPROTECT(1);
  return out;
}

SEXP warp_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  // Mask `fn` with `fn_sym`. We dispatch in the global environment.
  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv, 4));
  Rf_defineVar(fn_sym, fn, mask);

  SEXP out = warp_eval_mask_n_impl(fn_sym, syms, args, mask);

  UNPROTECT(1);
  return out;
}

SEXP warp_dispatch1(SEXP fn_sym, SEXP fn,
                         SEXP x_sym, SEXP x) {
  SEXP syms[2] = { x_sym, NULL };
  SEXP args[2] = { x, NULL };
  return warp_dispatch_n(fn_sym, fn, syms, args);
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
SEXP as_posixct_from_posixlt(SEXP x) {
  return warp_dispatch1(
    syms_as_posixct_from_posixlt, fns_as_posixct_from_posixlt,
    syms_x, x
  );
}

// [[ include("utils.h") ]]
SEXP as_posixlt_from_posixct(SEXP x) {
  return warp_dispatch1(
    syms_as_posixlt_from_posixct, fns_as_posixlt_from_posixct,
    syms_x, x
  );
}

// [[ include("utils.h") ]]
SEXP as_date(SEXP x) {
  return warp_dispatch1(
    syms_as_date, fns_as_date,
    syms_x, x
  );
}

// -----------------------------------------------------------------------------

void warp_init_utils(SEXP ns) {
  warp_ns_env = ns;

  syms_x = Rf_install("x");
  syms_tzone = Rf_install("tzone");
  syms_class = Rf_install("class");

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  syms_as_posixct_from_posixlt = Rf_install("as_posixct_from_posixlt");
  syms_as_posixlt_from_posixct = Rf_install("as_posixlt_from_posixct");
  syms_as_date = Rf_install("as_date");

  fns_as_posixct_from_posixlt = r_env_get(warp_ns_env, syms_as_posixct_from_posixlt);
  fns_as_posixlt_from_posixct = r_env_get(warp_ns_env, syms_as_posixlt_from_posixct);
  fns_as_date = r_env_get(warp_ns_env, syms_as_date);

  classes_data_frame = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(classes_data_frame);
  SET_STRING_ELT(classes_data_frame, 0, Rf_mkChar("data.frame"));

  classes_posixct = Rf_allocVector(STRSXP, 2);
  R_PreserveObject(classes_posixct);
  SET_STRING_ELT(classes_posixct, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(classes_posixct, 1, Rf_mkChar("POSIXt"));

  strings_start_stop = Rf_allocVector(STRSXP, 2);
  R_PreserveObject(strings_start_stop);
  SET_STRING_ELT(strings_start_stop, 0, Rf_mkChar("start"));
  SET_STRING_ELT(strings_start_stop, 1, Rf_mkChar("stop"));

  // Holds the CHARSXP objects because they can be garbage collected
  chars = Rf_allocVector(STRSXP, 4);
  R_PreserveObject(chars);

  char_posixlt = Rf_mkChar("POSIXlt");
  SET_STRING_ELT(chars, 0, char_posixlt);

  char_posixct = Rf_mkChar("POSIXct");
  SET_STRING_ELT(chars, 1, char_posixct);

  char_posixt = Rf_mkChar("POSIXt");
  SET_STRING_ELT(chars, 2, char_posixt);

  char_date = Rf_mkChar("Date");
  SET_STRING_ELT(chars, 3, char_date);
}
