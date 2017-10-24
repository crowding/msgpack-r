#ifndef OPTS_H
#define OPTS_H

#include <R.h>
#include <Rinternals.h>

/* To hand options down from R code into the callbacks, I attach this
   options structure to the CWpack state structures. 
   
   These structures exist at R level as the contents of some RAWSXP.
   This seems safe enough. But part of these structures are also
   pointers to SEXPs. I cover for this by also holding the SEXP
   pointers as an attrubute. But this feels tacky.
*/

typedef struct unpack_opts {
  SEXP dict;
  int use_df;
  int simplify;
  SEXP package;
  unsigned int max_depth;
  unsigned long max_pending;

  /* state variables used in unpack */
  SEXP buf;
  PROTECT_INDEX buf_index;
  unsigned int depth;
  unsigned long pending;
  SEXP underflow_handler;
  
} unpack_opts;


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
