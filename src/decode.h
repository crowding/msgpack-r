#ifndef DECODE_H
#define DECODE_H

#include "vadr.h"
#include "cwpack.h"

SEXP extract_sexp(cw_unpack_context *);
int init_unpack_context(cw_unpack_context *, SEXP, SEXP, unsigned long);

SEXP _unpack_msg(SEXP, SEXP);
SEXP _unpack_msg_partial(SEXP, SEXP, SEXP);
SEXP _unpack_opts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif
