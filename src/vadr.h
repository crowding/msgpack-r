#ifndef _VADR_H
#define _VADR_H

#include <R.h>
#include <Rinternals.h>

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define ASSERT(X) {if (! (X)) { error("%s: expected that (%s)", __FUNCTION__, #X); }}

#define LOGICAL_ELT(O, I) (LOGICAL(O)[I])
#define INTEGER_ELT(O, I) (INTEGER(O)[I])
#define REAL_ELT(O, I) (REAL(O)[I])

#define LOG(FMT, ...) Rprintf(FMT " (%s:%d)\n", ##__VA_ARGS__, __FUNCTION__, __LINE__)

#define WARN_ONCE(...) ({                                         \
      static long last_warned = 0;                                \
      if (last_warned < calls) {                                  \
        last_warned = calls;                                      \
        warning(__VA_ARGS__);                                     \
      }                                                           \
    })

void assert_type(SEXP, SEXPTYPE);
void assert_type3(SEXP, SEXPTYPE, const char *);

const char *decode_return_code(int);

#endif
