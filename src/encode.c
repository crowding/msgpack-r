#include "cwpack.h"
#include "vadr.h"

void pack_sexp(cw_pack_context* cxt, SEXP input);
int handle_overflow(cw_pack_context *, unsigned long);
SEXP copy_to_new_raw(SEXP, unsigned long, unsigned long);

void pack_sexp(cw_pack_context *, SEXP);
void pack_singleton(cw_pack_context *, SEXP);
void pack_vector(cw_pack_context*, SEXP);

void pack_logical(cw_pack_context *, int);
void pack_integer(cw_pack_context *, int);
void pack_real(cw_pack_context *, double);
void pack_string(cw_pack_context *, SEXP);
void pack_raw(cw_pack_context *, SEXP);

SEXP current = NULL;
int current_index = 0;

SEXP _packb(SEXP input, SEXP warn, SEXP compatible, SEXP all_arrays) {
  cw_pack_context cxt;

  if (current != NULL) {
    // hmmmm.
    current = NULL;
    error("Recursive use of _packb is not allowed");
  }
  /* allocate a raw SEXP, set global "current" so that we can reallocate  */
  unsigned long len = 256;
  PROTECT_WITH_INDEX(current = allocVector(RAWSXP, len), &current_index);
  
  cw_pack_context_init(&cxt, RAW(current), len, &handle_overflow);
  cw_pack_set_compatibility(&cxt, asLogical(compatible));

  pack_sexp(&cxt, input);
  
  SEXP out = current;
  current = NULL;
  SETLENGTH(out, (cxt.current - cxt.start) / sizeof(Rbyte));
  UNPROTECT(1);
  return out;
}

int handle_overflow(cw_pack_context *cxt, unsigned long more) {
  /* allocate a vector twice as big, copy data into the new vector, and update context. */
  unsigned long newlen = LENGTH(current);
  unsigned long req = LENGTH(current) + more;
  while (newlen < req) newlen *= 2;
  REPROTECT(current = copy_to_new_raw(current, newlen, LENGTH(current)),
            current_index);

  // update the context structure to point to the new buf
  cxt->current = cxt->current - cxt->start + RAW(current);
  cxt->end = RAW(current) + LENGTH(current) * sizeof(Rbyte);
  cxt->start = RAW(current);
  return 1;
}

SEXP copy_to_new_raw(SEXP from, unsigned long new_len, unsigned long copy_len) {
  assert_type(from, RAWSXP);
  SEXP to = PROTECT(allocVector(RAWSXP, new_len));
  memcpy(RAW(to), RAW(from), MIN(new_len, copy_len) * sizeof(Rbyte));
  UNPROTECT(1);
  return to;
}

void pack_sexp(cw_pack_context* cxt, SEXP dat) {
  if (isVector(dat)) {
    if (LENGTH(dat) == 1) {
      pack_singleton(cxt, dat);
    } else {
      pack_vector(cxt, dat);
    }
  } else {
    switch (TYPEOF(dat)) {

    case NILSXP:
      cw_pack_nil(cxt); break;

    default:
      current = NULL;
      error("can't pack a %s", type2char(TYPEOF(dat)));
    }
  }
}

void pack_singleton(cw_pack_context *cxt, SEXP dat) {
  switch (TYPEOF(dat)) {
  case LGLSXP:
    pack_logical(cxt, LOGICAL(dat)[0]);
    break;
    
  case INTSXP:
    pack_integer(cxt, INTEGER(dat)[0]);
    break;
    
  case REALSXP:
    pack_real(cxt, REAL(dat)[0]);
    break;
    
  case STRSXP:
    pack_string(cxt, STRING_ELT(dat, 0));
    break;

  case RAWSXP:
    pack_raw(cxt, dat);
    break;
    
  case VECSXP:
    pack_vector(cxt, dat);
    break;
 
  default:
    current = NULL;
    error("can't pack a singleton %s", type2char(TYPEOF(dat)));
  }
}

// genericity via macros, oh dear
#define LOGICAL_ELT(O, I) (LOGICAL(O)[i])
#define INTEGER_ELT(O, I) (INTEGER(O)[i])
#define REAL_ELT(O, I) (REAL(O)[i])

#define PACK_VECTOR(CXT, X, ACCESSOR, STORE) {     \
    int len = LENGTH(X);                           \
    cw_pack_array_size(cxt, len);                  \
    for (int i = 0; i < len; i++) {                \
      STORE(CXT, ACCESSOR(X, i));                  \
    }                                              \
  }

void pack_vector(cw_pack_context *cxt, SEXP x) {
  ASSERT(isVector(x));
  
  switch(TYPEOF(x)) {

  case RAWSXP:
    pack_raw(cxt, x);
    break;
    
  case LGLSXP:
    PACK_VECTOR(cxt, x, LOGICAL_ELT, pack_logical);
    break;
    
  case INTSXP:
    PACK_VECTOR(cxt, x, INTEGER_ELT, pack_integer);
    break;

  case REALSXP:
    PACK_VECTOR(cxt, x, REAL_ELT, pack_real);
    break;

  case VECSXP:
    PACK_VECTOR(cxt, x, VECTOR_ELT, pack_sexp);
    break;

  case STRSXP:
    PACK_VECTOR(cxt, x, STRING_ELT, pack_string);
    break;

  default:
    current = NULL;
    error("Don't know how to pack a %s vector", type2char(TYPEOF(x)));
  }
}

void pack_logical(cw_pack_context *cxt, int x) {
  if (x == NA_LOGICAL) {
    cw_pack_nil(cxt);
  } else if (x) {
    cw_pack_true(cxt);
  } else {
    cw_pack_false(cxt);
  }
}

void pack_integer(cw_pack_context *cxt, int x) {
  if (x == NA_INTEGER) {
    cw_pack_nil(cxt);
  } else {
    cw_pack_signed(cxt, x);
  }
}

void pack_real(cw_pack_context *cxt, double x) {
  // Numeric NA and NAN need no handling for reals
  cw_pack_real(cxt, x);
}

void pack_string(cw_pack_context *cxt, SEXP x) {
  assert_type(x, CHARSXP);
  if (x == NA_STRING) {
    cw_pack_nil(cxt);
  } else {
    int len = R_nchar(x, Bytes, 0, 0, "");
    cw_pack_str(cxt, CHAR(x), len);
  }
}

void pack_raw(cw_pack_context *cxt, SEXP x) {
  assert_type(x, RAWSXP);
  cw_pack_bin(cxt, RAW(x), LENGTH(x));
}
