#ifndef ENCODE_H
#define ENCODE_H

#include "vadr.h"
#include "cwpack.h"

SEXP _pack_opts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP _pack_msg(SEXP, SEXP);

void pack_sexp(cw_pack_context *, SEXP);
int init_pack_context(cw_pack_context *, SEXP);

#endif
 
