#ifndef ENCODE_H
#define ENCODE_H

#include "vadr.h"
#include "cwpack.h"

void pack_sexp(cw_pack_context *, SEXP);
int init_pack_context(cw_pack_context *, SEXP);

#endif
 
