#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h>

#include "dispatch.h"

extern "C" {

static const R_CallMethodDef callMethods[] = {
    {"trace_dispatch_call", (DL_FUNC)&trace_dispatch_call, 3}, 
    {NULL, NULL, 0}
};

void R_init_generatr(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
}
