#ifndef DISPATCH_H
#define DISPATCH_H

#include <Rinternals.h>

extern "C" {
SEXP trace_dispatch_call(SEXP fun, SEXP args, SEXP rho);
}

#endif /* DISPATCH_H */
