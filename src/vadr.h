#ifndef _VADR_H
#define _VADR_H

#include <R.h>
#include <Rinternals.h>

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define ASSERT(X) {if (! (X)) { error("%s: expected that (%s) @%s:%d\n", __func__, #X, __FILE__, __LINE__); }}

#define LOGICAL_ELT(O, I) (LOGICAL(O)[I])
#define INTEGER_ELT(O, I) (INTEGER(O)[I])
#define REAL_ELT(O, I) (REAL(O)[I])

#define WARN_ONCE(...) ({                       \
      static long last_warned = 0;              \
      if (last_warned < calls) {                \
        last_warned = calls;                    \
        warning(__VA_ARGS__);                   \
      }                                         \
    })

#undef DEBUG 

#ifdef DEBUG
#define LOG(FMT, ...) Rprintf("%s: "  FMT " @%s:%d\n",   \
                              __func__, ##__VA_ARGS__, __FILE__, __LINE__)
#else
#define LOG(...) NULL
#endif

#define assert_type(x, type) assert_type3(x, type, __func__)
#define assert_type3(x, type, where) assert_type5(x, type, where, __FILE__, __LINE__)

inline void assert_type5(SEXP x, SEXPTYPE type, const char *where,
                         const char *file, int line) {
  if (TYPEOF(x) != type) {
    error("Expected %s in %s, got %s (%s:%d)",
          type2char(type), where, type2char(TYPEOF(x)), file, line);
  }
}

const char *decode_return_code(int);

#endif
