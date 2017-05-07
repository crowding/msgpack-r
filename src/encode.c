#include "cwpack.h"
#include <R.h>
#include <Rinternals.h>

#define MIN(x,y) ((x) < (y) ? (x) : (y))

void pack_sexp(cw_pack_context* cxt, SEXP input);
int handle_overflow(cw_pack_context *, unsigned long);
SEXP copy_to_new_vector(SEXP, unsigned long, unsigned long);

SEXP current;
int current_index;

SEXP _packb(SEXP input, SEXP warn, SEXP compatible, SEXP use_arrays) {
  cw_pack_context cxt;
  
  /* allocate a raw SEXP, set global "current" so that we can reallocate  */
  unsigned long len = 256;
  PROTECT_WITH_INDEX(current = allocVector(RAWSXP, len), &current_index);
  
  cw_pack_context_init(&cxt, RAW(current), len, &handle_overflow);
  cw_pack_set_compatibility(&cxt, asLogical(compatible));

  pack_sexp(&cxt, input);
  
  SEXP out = current;
  current = R_NilValue;
  UNPROTECT(1);

  SETLENGTH(out, (cxt.current - cxt.start) / sizeof(Rbyte));
  
  return out;
}

int handle_overflow(cw_pack_context *cxt, unsigned long more) {
  // allocate a vector twice as big and copy data into the new vector.
  unsigned long newlen = LENGTH(current);
  unsigned long req = LENGTH(current) + more;
  while (newlen < req) newlen *= 2;
  REPROTECT(current = copy_to_new_vector(current, newlen, LENGTH(current)),
            current_index);

  // update the context structure to point to the new buf
  cxt->current = cxt->current - cxt->start + RAW(current);
  cxt->end = RAW(current) + LENGTH(current) * sizeof(Rbyte);
  cxt->start = RAW(current);
  return 1;
}

SEXP copy_to_new_vector(SEXP from, unsigned long new_len, unsigned long copy_len) {
  SEXP to = PROTECT(allocVector(RAWSXP, new_len));
  memcpy(RAW(to), RAW(from), MIN(new_len, copy_len) * sizeof(Rbyte));
  UNPROTECT(1);
  return to;
}

void pack_singleton(cw_pack_context*, SEXP);
void pack_vector(cw_pack_context*, SEXP);

void pack_sexp(cw_pack_context* cxt, SEXP dat) {
  if (isVector(dat)) {
    if (LENGTH(dat) == 1) {
      pack_singleton(cxt, dat);
    } else {
      pack_vector(cxt, dat);
    }
  } else {
    switch (TYPEOF(dat)) {
    case NILSXP: cw_pack_nil(cxt); break;
    default: error("can't pack a %s", type2char(TYPEOF(dat)));
    }
  }
}

void pack_singleton(cw_pack_context *cxt, SEXP dat) {
  switch (TYPEOF(dat)) {
  case LGLSXP:
    if (LOGICAL(dat)[0] == NA_LOGICAL) {
      cw_pack_nil(cxt);
    } else if (LOGICAL(dat)[0]) {
      cw_pack_true(cxt);
    } else {
      cw_pack_false(cxt);
    }
    return;
    
  case INTSXP:
    if (INTEGER(dat)[0] == NA_INTEGER) {
      cw_pack_nil(cxt);
    } else {
      cw_pack_signed(cxt, INTEGER(dat)[0]);
    }
    return;
    
  case REALSXP:
    // Numeric NA and NAN should be represented faithfully
    cw_pack_real(cxt, REAL(dat)[0]);
    return;
    
  case STRSXP:
    if (STRING_ELT(dat, 0) == NA_STRING) {
      cw_pack_nil(cxt);
    } else {
      int len = R_nchar(STRING_ELT(dat, 0), Bytes, 0, 0, "");
      cw_pack_str(cxt, CHAR(STRING_ELT(dat, 0)), len);
    }
    return;
    
  default:
    error("can't pack a singleton %s", type2char(TYPEOF(dat)));
    return;
  }
}

void pack_vector(cw_pack_context *cxt, SEXP dat) {
  switch(TYPEOF(dat)) {
  default: error("can't pack a vector %s", type2char(TYPEOF(dat))); return;
  }
}


