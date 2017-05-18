#ifndef OPTS_H
#define OPTS_H

#include <RInternals.h>

typedef struct unpack_opts {
  SEXP dict;
  int use_df;
  int simplify;
  SEXP package;

  /* state variables used in unpack */
  SEXP buf;
  int buf_index;
  int depth;

  /* State variables used in read */
  SEXP conn;
  unsigned long msg_start;
  SEXP read_size;
  
} unpack_opts;

#include <Rinternals.h>

typedef struct pack_opts {

  int as_is;
  int compatible;
  int use_dict;
  long max_size;
  long buf_size;

  /* state used by pack / write */
  SEXP buf;
  PROTECT_INDEX buf_index;
  SEXP package;

  /* state used by write */
  SEXP conn;

} pack_opts;

#endif
