#ifndef DECODE_H
#define DECODE_H

#include "vadr.h"
#include "cwpack.h"

SEXP extract_sexp(cw_unpack_context *);
int init_unpack_context(cw_unpack_context *, SEXP, SEXP, unsigned long);

#endif
