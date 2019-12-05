#include "utils.h"

// -----------------------------------------------------------------------------

SEXP timewarp_ns_env = NULL;

SEXP syms_x = NULL;
SEXP syms_components = NULL;

SEXP syms_time_get = NULL;
SEXP syms_as_posixct_from_posixlt = NULL;
SEXP syms_as_date = NULL;

SEXP fns_time_get = NULL;
SEXP fns_as_posixct_from_posixlt = NULL;
SEXP fns_as_date = NULL;

SEXP strings_year = NULL;
SEXP strings_year_month = NULL;

SEXP strings = NULL;
SEXP strings_posixlt = NULL;
SEXP strings_posixct = NULL;
SEXP strings_posixt = NULL;
SEXP strings_date = NULL;

// -----------------------------------------------------------------------------

void never_reached(const char* fn) {
  Rf_error("Internal error in `%s()`: Reached the unreachable.", fn);
}

enum timewarp_class_type time_class_type(SEXP x);
static enum timewarp_class_type time_class_type_impl(SEXP klass);
static const char* class_type_as_str(enum timewarp_class_type type);

// [[ register() ]]
SEXP timewarp_class_type(SEXP x) {
  return Rf_mkString(class_type_as_str(time_class_type(x)));
}

enum timewarp_class_type time_class_type(SEXP x) {
  if (!OBJECT(x)) {
    return timewarp_class_unknown;
  }

  SEXP klass = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  enum timewarp_class_type type = time_class_type_impl(klass);

  UNPROTECT(1);
  return type;
}

static enum timewarp_class_type time_class_type_impl(SEXP klass) {
  int n = Rf_length(klass);
  SEXP const* p_klass = STRING_PTR(klass);

  p_klass += n - 2;
  SEXP butlast = *p_klass++;
  SEXP last = *p_klass++;

  if (last == strings_date) {
    return timewarp_class_date;
  }

  if (last == strings_posixt) {
    if (butlast == strings_posixlt) {
      return timewarp_class_posixlt;
    } else if (butlast == strings_posixct) {
      return timewarp_class_posixct;
    }
  }

  return timewarp_class_unknown;
}

static const char* class_type_as_str(enum timewarp_class_type type) {
  switch (type) {
  case timewarp_class_date: return "date";
  case timewarp_class_posixct: return "posixct";
  case timewarp_class_posixlt: return "posixlt";
  case timewarp_class_unknown: return "unknown";
  }
  never_reached("class_type_as_str");
}

// -----------------------------------------------------------------------------

// TODO - Could be lossy...really should use vctrs? Callable from C?
int pull_every(SEXP every) {
  if (Rf_length(every) != 1) {
    r_error("pull_every", "`every` must have size 1, not %i", Rf_length(every));
  }

  switch (TYPEOF(every)) {
  case INTSXP: return INTEGER(every)[0];
  case REALSXP: return Rf_asInteger(every);
  default: r_error("pull_every", "`every` must be integer-ish, not %s", Rf_type2char(TYPEOF(every)));
  }
}

// -----------------------------------------------------------------------------

static bool str_equal(const char* x, const char* y) {
  return strcmp(x, y) == 0;
}

// [[ include("utils.h") ]]
enum timewarp_group_type as_group_type(SEXP by) {
  if (TYPEOF(by) != STRSXP || Rf_length(by) != 1) {
    Rf_errorcall(R_NilValue, "`by` must be a single string.");
  }

  const char* type = CHAR(STRING_ELT(by, 0));

  if (str_equal(type, "year") || str_equal(type, "years") || str_equal(type, "yearly")) {
    return timewarp_group_year;
  }

  if (str_equal(type, "quarter") || str_equal(type, "quarters") || str_equal(type, "quarterly")) {
    return timewarp_group_quarter;
  }

  if (str_equal(type, "month") || str_equal(type, "months") || str_equal(type, "monthly")) {
    return timewarp_group_month;
  }

  if (str_equal(type, "week") || str_equal(type, "weeks") || str_equal(type, "weekly")) {
    return timewarp_group_week;
  }

  if (str_equal(type, "day") || str_equal(type, "days") || str_equal(type, "daily")) {
    return timewarp_group_day;
  }

  if (str_equal(type, "hour") || str_equal(type, "hours") || str_equal(type, "hourly")) {
    return timewarp_group_hour;
  }

  if (str_equal(type, "minute") || str_equal(type, "minutes") || str_equal(type, "minutely")) {
    return timewarp_group_minute;
  }

  if (str_equal(type, "second") || str_equal(type, "seconds") || str_equal(type, "secondly")) {
    return timewarp_group_second;
  }

  if (str_equal(type, "millisecond") || str_equal(type, "milliseconds")) {
    return timewarp_group_millisecond;
  }

  Rf_errorcall(R_NilValue, "Unknown `by` value '%s'.", type);
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

  Rf_errorcall(R_NilValue, "In C function `%s()`:, %s", where, buf);
}

#undef BUFSIZE

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

static SEXP timewarp_eval_mask_n_impl(SEXP fn, SEXP* syms, SEXP* args, SEXP mask) {
  SEXP call = PROTECT(r_call(fn, syms, syms));

  while (*syms) {
    Rf_defineVar(*syms, *args, mask);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(call, mask);

  UNPROTECT(1);
  return out;
}

SEXP timewarp_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  // Mask `fn` with `fn_sym`. We dispatch in the global environment.
  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv, 4));
  Rf_defineVar(fn_sym, fn, mask);

  SEXP out = timewarp_eval_mask_n_impl(fn_sym, syms, args, mask);

  UNPROTECT(1);
  return out;
}

SEXP timewarp_dispatch2(SEXP fn_sym, SEXP fn,
                         SEXP x_sym, SEXP x,
                         SEXP y_sym, SEXP y) {
  SEXP syms[3] = { x_sym, y_sym, NULL };
  SEXP args[3] = { x, y, NULL };
  return timewarp_dispatch_n(fn_sym, fn, syms, args);
}

SEXP timewarp_dispatch1(SEXP fn_sym, SEXP fn,
                         SEXP x_sym, SEXP x) {
  SEXP syms[2] = { x_sym, NULL };
  SEXP args[2] = { x, NULL };
  return timewarp_dispatch_n(fn_sym, fn, syms, args);
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
SEXP time_get(SEXP x, SEXP components) {
  return timewarp_dispatch2(
    syms_time_get, fns_time_get,
    syms_x, x,
    syms_components, components
  );
}

// [[ include("utils.h") ]]
SEXP as_posixct_from_posixlt(SEXP x) {
  return timewarp_dispatch1(
    syms_as_posixct_from_posixlt, fns_as_posixct_from_posixlt,
    syms_x, x
  );
}

// [[ include("utils.h") ]]
SEXP as_date(SEXP x) {
  return timewarp_dispatch1(
    syms_as_date, fns_as_date,
    syms_x, x
  );
}

// -----------------------------------------------------------------------------

void timewarp_init_utils(SEXP ns) {
  timewarp_ns_env = ns;

  syms_x = Rf_install("x");
  syms_components = Rf_install("components");

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  syms_time_get = Rf_install("time_get");
  syms_as_posixct_from_posixlt = Rf_install("as_posixct_from_posixlt");
  syms_as_date = Rf_install("as_date");

  fns_time_get = r_env_get(timewarp_ns_env, syms_time_get);
  fns_as_posixct_from_posixlt = r_env_get(timewarp_ns_env, syms_as_posixct_from_posixlt);
  fns_as_date = r_env_get(timewarp_ns_env, syms_as_date);

  strings_year = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_year);
  SET_STRING_ELT(strings_year, 0, Rf_mkChar("year"));

  strings_year_month = Rf_allocVector(STRSXP, 2);
  R_PreserveObject(strings_year_month);
  SET_STRING_ELT(strings_year_month, 0, Rf_mkChar("year"));
  SET_STRING_ELT(strings_year_month, 1, Rf_mkChar("month"));

  // Holds the CHARSXP objects because they can be garbage collected
  strings = Rf_allocVector(STRSXP, 4);
  R_PreserveObject(strings);

  strings_posixlt = Rf_mkChar("POSIXlt");
  SET_STRING_ELT(strings, 0, strings_posixlt);

  strings_posixct = Rf_mkChar("POSIXct");
  SET_STRING_ELT(strings, 1, strings_posixct);

  strings_posixt = Rf_mkChar("POSIXt");
  SET_STRING_ELT(strings, 2, strings_posixt);

  strings_date = Rf_mkChar("Date");
  SET_STRING_ELT(strings, 3, strings_date);
}
