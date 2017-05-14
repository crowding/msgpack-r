#ifndef ENCODE_H
#define ENCODE_H

#include <Rinternals.h>

typedef struct pack_opts {
  SEXP buf;
  PROTECT_INDEX buf_index;

  SEXP package_env;
  int as_is;
  long max_size;
  int use_dict;
} pack_opts;


SEXP _packb(SEXP input, SEXP compatible, SEXP as_is,
            SEXP package, SEXP max_size, SEXP use_dicts);

#endif
