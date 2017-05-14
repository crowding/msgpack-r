#ifndef DECODE_H
#define DECODE_H

typedef struct unpack_opts {
  SEXP dict;
  int use_df;
  int depth;
  SEXP package;
} unpack_opts;

SEXP _unpackb(SEXP dat, SEXP dict, SEXP use_df, SEXP package);

#endif
