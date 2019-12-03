#include "utils.h"

// -----------------------------------------------------------------------------

SEXP timeslide_ns_env = NULL;

SEXP syms_x = NULL;
SEXP syms_components = NULL;

SEXP syms_time_get = NULL;

SEXP fns_time_get = NULL;

SEXP strings_year = NULL;
SEXP strings_year_month = NULL;

SEXP strings = NULL;
SEXP strings_posixlt = NULL;
SEXP strings_posixct = NULL;
SEXP strings_posixt = NULL;
SEXP strings_date = NULL;

// -----------------------------------------------------------------------------

inline void never_reached(const char* fn) {
  Rf_error("Internal error in `%s()`: Reached the unreachable.", fn);
}

enum timeslide_class_type time_class_type(SEXP x);
static enum timeslide_class_type time_class_type_impl(SEXP klass);
static const char* class_type_as_str(enum timeslide_class_type type);

// [[ register() ]]
SEXP timeslide_class_type(SEXP x) {
  return Rf_mkString(class_type_as_str(time_class_type(x)));
}

enum timeslide_class_type time_class_type(SEXP x) {
  if (!OBJECT(x)) {
    return timeslide_class_unknown;
  }

  SEXP klass = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  enum timeslide_class_type type = time_class_type_impl(klass);

  UNPROTECT(1);
  return type;
}

static enum timeslide_class_type time_class_type_impl(SEXP klass) {
  int n = Rf_length(klass);
  SEXP const* p_klass = STRING_PTR(klass);

  p_klass += n - 2;
  SEXP butlast = *p_klass++;
  SEXP last = *p_klass++;

  if (last == strings_date) {
    return timeslide_class_date;
  }

  if (last == strings_posixt) {
    if (butlast == strings_posixlt) {
      return timeslide_class_posixlt;
    } else if (butlast == strings_posixct) {
      return timeslide_class_posixct;
    }
  }

  return timeslide_class_unknown;
}

static const char* class_type_as_str(enum timeslide_class_type type) {
  switch (type) {
  case timeslide_class_date: return "date";
  case timeslide_class_posixct: return "posixct";
  case timeslide_class_posixlt: return "posixlt";
  case timeslide_class_unknown: return "unknown";
  }
  never_reached("class_type_as_str");
}

// -----------------------------------------------------------------------------

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

static SEXP timeslide_eval_mask_n_impl(SEXP fn, SEXP* syms, SEXP* args, SEXP mask) {
  SEXP call = PROTECT(r_call(fn, syms, syms));

  while (*syms) {
    Rf_defineVar(*syms, *args, mask);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(call, mask);

  UNPROTECT(1);
  return out;
}

SEXP timeslide_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  // Mask `fn` with `fn_sym`. We dispatch in the global environment.
  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv, 4));
  Rf_defineVar(fn_sym, fn, mask);

  SEXP out = timeslide_eval_mask_n_impl(fn_sym, syms, args, mask);

  UNPROTECT(1);
  return out;
}

SEXP timeslide_dispatch2(SEXP fn_sym, SEXP fn,
                         SEXP x_sym, SEXP x,
                         SEXP y_sym, SEXP y) {
  SEXP syms[3] = { x_sym, y_sym, NULL };
  SEXP args[3] = { x, y, NULL };
  return timeslide_dispatch_n(fn_sym, fn, syms, args);
}

// -----------------------------------------------------------------------------

// [[ include("utils.h") ]]
SEXP time_get(SEXP x, SEXP components) {
  return timeslide_dispatch2(
    syms_time_get, fns_time_get,
    syms_x, x,
    syms_components, components
  );
}

// -----------------------------------------------------------------------------

void timeslide_init_utils(SEXP ns) {
  timeslide_ns_env = ns;

  syms_x = Rf_install("x");
  syms_components = Rf_install("components");

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  syms_time_get = Rf_install("time_get");

  fns_time_get = r_env_get(timeslide_ns_env, syms_time_get);

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
