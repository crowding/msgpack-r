#ifndef _VADR_H
#define _VADR_H

#include <R.h>
#include <Rinternals.h>

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define ASSERT(X) {if (! (X)) { error("expected that %s", #X); }}
#define ASSERT2(X, MSG) {if (! (X)) { error("expected that %s in %s", #X, MSG); }}

#define LOGICAL_ELT(O, I) (LOGICAL(O)[I])
#define INTEGER_ELT(O, I) (INTEGER(O)[I])
#define REAL_ELT(O, I) (REAL(O)[I])

#define LOG Rprintf

#define WARN_ONCE warning

void assert_type(SEXP, SEXPTYPE);
void assert_type3(SEXP, SEXPTYPE, const char *);

#endif
