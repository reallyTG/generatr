#include <string>
#include <unordered_map>
#include <unordered_set>

#include <Rdyntrace.h>

#include "dispatch.h"

typedef struct {
  SEXP current_arg;
  std::unordered_map<SEXP, std::unordered_set<std::string>> args;
} DispatchState;

// from memory.c
const std::string sexp2char(SEXP v) {
  switch (TYPEOF(v)) {
  case NILSXP:
    return "NILSXP";
  case SYMSXP:
    return "SYMSXP";
  case LISTSXP:
    return "LISTSXP";
  case CLOSXP:
    return "CLOSXP";
  case ENVSXP:
    return "ENVSXP";
  case PROMSXP:
    return "PROMSXP";
  case LANGSXP:
    return "LANGSXP";
  case SPECIALSXP:
    return "SPECIALSXP";
  case BUILTINSXP:
    return "BUILTINSXP";
  case CHARSXP:
    return "CHARSXP";
  case LGLSXP:
    return "LGLSXP";
  case INTSXP:
    return "INTSXP";
  case REALSXP:
    return "REALSXP";
  case CPLXSXP:
    return "CPLXSXP";
  case STRSXP:
    return "STRSXP";
  case DOTSXP:
    return "DOTSXP";
  case ANYSXP:
    return "ANYSXP";
  case VECSXP:
    return "VECSXP";
  case EXPRSXP:
    return "EXPRSXP";
  case BCODESXP:
    return "BCODESXP";
  case EXTPTRSXP:
    return "EXTPTRSXP";
  case WEAKREFSXP:
    return "WEAKREFSXP";
  case S4SXP:
    return "S4SXP";
  case RAWSXP:
    return "RAWSXP";
  case NEWSXP:
    return "NEWSXP"; /* should never happen */
  case FREESXP:
    return "FREESXP";
  default:
    return "<unknown>";
  }
}

void S3_generic_entry(dyntracer_t *tracer, const char *generic,
                      const SEXP generic_method, const SEXP obj) {
  auto state = (DispatchState *)dyntracer_get_data(tracer);

  if (RTRACE(obj)) {
    state->current_arg = obj;
  } else {
    state->current_arg = R_NilValue;
  }
}

void S3_dispatch_entry_callback(dyntracer_t *tracer, const char *generic,
                                const SEXP cls, const SEXP generic_method,
                                const SEXP specific_method,
                                const SEXP objects) {

  auto state = (DispatchState *)dyntracer_get_data(tracer);
  if (state->current_arg == R_NilValue) {
    return;
  }

  dyntrace_disable();

  std::string r;
  if (cls == R_NilValue) {
    r = "<default>";
  } else if (TYPEOF(cls) == STRSXP) {
    r = CHAR(STRING_ELT(cls, 0));
  } else {
    r = CHAR(cls);
  }

  r += "::";
  r += generic;
  state->args[state->current_arg].insert(r);

  dyntrace_enable();

  state->current_arg = R_NilValue;
}

void object_coerce(dyntracer_t *tracer, const SEXP x, const SEXP y) {
  if (!RTRACE(x)) {
    return;
  }

  auto state = (DispatchState *)dyntracer_get_data(tracer);
  std::string to = "as." + sexp2char(y);
  state->args[x].insert(to);
}

dyntracer_t *create_tracer() {
  dyntracer_t *tracer = dyntracer_create(NULL);

  dyntracer_set_S3_generic_entry_callback(
      tracer, [](dyntracer_t *tracer, const char *generic,
                 const SEXP generic_method, const SEXP obj) {
        S3_generic_entry(tracer, generic, generic_method, obj);
      });
  dyntracer_set_S3_dispatch_entry_callback(
      tracer, [](dyntracer_t *tracer, const char *generic, const SEXP cls,
                 const SEXP generic_method, const SEXP specific_method,
                 const SEXP objects) {
        S3_dispatch_entry_callback(tracer, generic, cls, generic_method,
                                   specific_method, objects);
      });
  dyntracer_set_object_coerce_callback(
      tracer, [](dyntracer_t *tracer, const SEXP x, const SEXP y) {
        object_coerce(tracer, x, y);
      });

  return tracer;
}

SEXP trace_dispatch_call(SEXP fun, SEXP args, SEXP rho) {
  // TODO: check args are not empty
  dyntracer_t *tracer = create_tracer();

  DispatchState state;

  dyntracer_set_data(tracer, (void *)&state);

  SEXP call = R_NilValue;
  for (int i = Rf_length(args) - 1; i >= 0; i--) {
    SEXP arg = VECTOR_ELT(args, i);
    SET_RTRACE(arg, 1);

    PROTECT(call);
    call = Rf_lcons(arg, call);
    UNPROTECT(1);
  }

  call = PROTECT(Rf_lcons(fun, call));

  dyntrace_enable();
  dyntrace_result_t result = dyntrace_trace_code(tracer, call, rho);
  dyntrace_disable();

  UNPROTECT(1); // call

  SEXP table = PROTECT(Rf_allocVector(VECSXP, Rf_length(args)));
  for (int i = 0; i < Rf_length(args); i++) {
    SEXP arg = VECTOR_ELT(args, i);
    // reset the flag
    SET_RTRACE(arg, 0);
    SEXP v;

    auto it = state.args.find(arg);
    if (it != state.args.end()) {
      auto r = it->second;

      v = PROTECT(Rf_allocVector(STRSXP, r.size()));
      int j = 0;
      for (auto itr = r.begin(); itr != r.end(); itr++, j++) {
        SET_STRING_ELT(v, j, Rf_mkChar(itr->c_str()));
      }
    } else {
      v = PROTECT(Rf_allocVector(STRSXP, 0));
    }

    SET_VECTOR_ELT(table, i, v);
    UNPROTECT(1); // v
  }

  if (TYPEOF(fun) == CLOSXP) {
    SEXP formals = FORMALS(fun);
    if (formals != R_NilValue) {
      Rf_setAttrib(table, R_NamesSymbol, Rf_getAttrib(formals, R_NamesSymbol));
    }
  }

  const char *names[] = {"status", "result", "dispatch", ""};
  SEXP ret = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(ret, 0, Rf_ScalarInteger(result.error_code));
  SET_VECTOR_ELT(ret, 1, result.error_code == 0 ? result.value : R_NilValue);
  SET_VECTOR_ELT(ret, 2, table);
  // if (R_CollectWarnings > 0) {
  //   SET_VECTOR_ELT(ret, 3, Rf_getAttrib(R_Warnings, R_NamesSymbol));
  // } else {
  //   SET_VECTOR_ELT(ret, 3, R_NaString);
  // }
  Rf_setAttrib(ret, R_ClassSymbol, Rf_mkString("dispatch_result"));
  UNPROTECT(2); // ret + table

  return ret;
}
