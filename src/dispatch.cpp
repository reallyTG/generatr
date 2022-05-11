#include <string>
#include <unordered_map>
#include <unordered_set>

#include <Rdyntrace.h>

#include "dispatch.h"

typedef std::unordered_map<SEXP, std::unordered_set<std::string>> DispatchState;

void S3_dispatch_exit_callback(dyntracer_t *tracer, const char *generic,
                                const SEXP cls, const SEXP generic_method,
                                const SEXP specific_method,
                                const SEXP objects, const SEXP ans) {

  auto state = (DispatchState *)dyntracer_get_data(tracer);
  dyntrace_disable();

  for (SEXP x = objects; x != R_NilValue; x = CDR(x)) {
    SEXP v = CAR(x);

    if (TYPEOF(v) == PROMSXP) {
      if (PRVALUE(v) != R_UnboundValue) {
        v = PRVALUE(v);
      } else {
        continue;
      }
    }

    if (!RTRACE(v)) {
      continue;
    }

    for (int i = 0; i < Rf_length(cls); i++) {
      SEXP c = STRING_ELT(cls, i);
      std::string r = CHAR(c);
      r += "::";
      r += generic;
      (*state)[v].insert(r);
    }
  }

  dyntrace_enable();
}

dyntracer_t *create_tracer() {
  dyntracer_t *tracer = dyntracer_create(NULL);

  dyntracer_set_S3_dispatch_exit_callback(
      tracer, [](dyntracer_t *tracer, const char *generic, const SEXP cls,
                 const SEXP generic_method, const SEXP specific_method,
                 const SEXP objects, const SEXP ans) {
        S3_dispatch_exit_callback(tracer, generic, cls, generic_method,
                                   specific_method, objects, ans);
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

    auto it = state.find(arg);
    if (it != state.end()) {
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
