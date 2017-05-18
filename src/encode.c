#include "cwpack.h"
#include "vadr.h"

int handle_overflow(cw_pack_context *, unsigned long);
SEXP copy_to_new_raw(SEXP, unsigned long, unsigned long);

void pack_sexp(cw_pack_context *, SEXP);

void pack_singleton(cw_pack_context *, SEXP);
void pack_vector(cw_pack_context*, SEXP);
void pack_named_vector(cw_pack_context*, SEXP, SEXP);

void pack_logical(cw_pack_context *, int);
void pack_integer(cw_pack_context *, int);
void pack_real(cw_pack_context *, double);
void pack_string(cw_pack_context *, SEXP);
void pack_raw(cw_pack_context *, SEXP);

static long calls = 0;

SEXP _pack_opts(SEXP compatible,
                SEXP as_is,
                SEXP use_dict,
                SEXP max_size,
                SEXP buf_size,
                SEXP package) {
  SEXP optsxp = PROTECT(allocVector(RAWSXP, sizeof(pack_opts)));
  pack_opts *opts = (pack_opts*)RAW(optsxp);
  opts->buf = R_NilValue;
  opts->buf_index = -1;
  opts->as_is = asLogical(as_is);
  opts->use_dict = asLogical(use_dict);
  opts->buf_size = asInteger(buf_size);
  double max_sized = asInteger(max_size);
  if (isinf(max_sized)) {
    opts->max_size = LONG_MAX;
  } else {
    opts->max_size = max_sized;
  }
  opts->compatible = asLogical(compatible);

  opts->package = package;
  /* also hang onto explicit refs for the SEXP values*/
  setAttrib(optsxp, install("refs"), CONS(package, R_NilValue));
  UNPROTECT(1);
  return optsxp;
}

            
int init_pack_context(cw_pack_context *cxt, SEXP opts) {
  ASSERT(TYPEOF(opts) == RAWSXP && LENGTH(opts) == sizeof(pack_opts));
  cxt->opts = (pack_opts *) RAW(opts);
  
  /* allocate a buffer */
  PROTECT_WITH_INDEX(cxt->opts->buf = allocVector(RAWSXP, cxt->opts->buf_size),
                     &cxt->opts->buf_index);
  cw_pack_context_init(cxt,
                       RAW(cxt->opts->buf),
                       LENGTH(cxt->opts->buf),
                       &handle_overflow);
  cw_pack_set_compatibility(cxt, cxt->opts->compatible);

  return 1;
}

            
SEXP _packb(SEXP input, SEXP opts) {
  calls++;
  cw_pack_context cxt;
  int protections = init_pack_context(&cxt, opts);
  
  pack_sexp(&cxt, input);

  if (cxt.return_code != CWP_RC_OK) {
    error("%s", decode_return_code(cxt.return_code));
  }

  SETLENGTH(cxt.opts->buf, (cxt.current - cxt.start) / sizeof(Rbyte));
  UNPROTECT(protections);
  return cxt.opts->buf;
}


int handle_overflow(cw_pack_context *cxt, unsigned long more) {
  /* allocate a vector twice as big, copy data into the new vector,
     and update context. */
  unsigned long newlen = LENGTH(cxt->opts->buf);
  unsigned long req = LENGTH(cxt->opts->buf) + more;

  if (req > cxt->opts->max_size) return CWP_RC_BUFFER_OVERFLOW;
  
  while (newlen < req) newlen *= 2;
  if (newlen > cxt->opts->max_size) {
    newlen = cxt->opts->max_size;
  }
  
  LOG("%d / %d bytes used, need %d more, resizing to %d\n",
      cxt->current - cxt->start,
      LENGTH(cxt->opts->buf),
      more,
      newlen);

  REPROTECT(cxt->opts->buf =
              copy_to_new_raw(cxt->opts->buf, newlen, LENGTH(cxt->opts->buf)),
            cxt->opts->buf_index);
  
  // update the context structure to point to the new buf
  cxt->current = cxt->current - cxt->start + RAW(cxt->opts->buf);
  cxt->end = RAW(cxt->opts->buf) + LENGTH(cxt->opts->buf) * sizeof(Rbyte);
  cxt->start = RAW(cxt->opts->buf);
  return 0;
}

            
SEXP copy_to_new_raw(SEXP from, unsigned long new_len, unsigned long copy_len) {
  assert_type(from, RAWSXP);
  SEXP to = PROTECT(allocVector(RAWSXP, new_len));
  memcpy(RAW(to), RAW(from), MIN(new_len, copy_len) * sizeof(Rbyte));
  UNPROTECT(1);
  return to;
}


int containsString(SEXP cl, const char *ch) {
  assert_type(cl, STRSXP);
  SEXP cmp = PROTECT(mkChar(ch));
  int found = 0;
  for (int i = 0; i < LENGTH(cl); i++) {
    if (STRING_ELT(cl, i) == cmp) {
      found = 1; break;
    }
  }
  UNPROTECT(1);
  return found;
}


void pack_sexp(cw_pack_context* cxt, SEXP dat) {
  int as_is_sto = cxt->opts->as_is;
  int unp = 0;

  /* check for asIs, classes */
  SEXP cl = getAttrib(dat, R_ClassSymbol);
  if ((cl) != R_NilValue) {
    if (containsString(cl, "AsIs")) {
      cxt->opts->as_is = TRUE;
    } else {
      /* Preprocess (unless in the middle of an AsIs) */
      LOG("Preprocessing a %s of class '%s'\n",
              type2char(TYPEOF(cxt->opts->package)),
              CHAR(STRING_ELT(cl, 0)));
      SEXP call = PROTECT(lang2(install("prepack"), dat));
      dat = PROTECT(eval(call, cxt->opts->package));
      unp += 2;
      
      /* Check if the preprocessor gave us an AsIs, but don't
         preprocess again */
      cl = getAttrib(dat, R_ClassSymbol);
      if (cl != R_NilValue && containsString(cl, "AsIs")) {
        cxt->opts->as_is = TRUE;
        LOG("Preprocessor returned an AsIs!");
      }
    }
  }
  
  if (isVector(dat)) {
    SEXP names = getAttrib(dat, R_NamesSymbol);
    if (names != R_NilValue && cxt->opts->use_dict) {
      pack_named_vector(cxt, dat, names);
    } else if (LENGTH(dat) == 1 && !cxt->opts->as_is) {
      pack_singleton(cxt, dat);
    } else {
      pack_vector(cxt, dat);
    }
  } else {
    switch (TYPEOF(dat)) {

    case NILSXP:
      cw_pack_nil(cxt); break;

    case ENVSXP:
      {
        SEXP args = PROTECT(lang4(install("as.list.environment"),
                                  dat,
                                  ScalarLogical(1),
                                  ScalarLogical(1)));
        SEXP list = PROTECT(eval(args, R_BaseEnv));
        unp += 2;
        SEXP names = getAttrib(list, R_NamesSymbol);
        pack_named_vector(cxt, list, names);
        break;
      }

    default:
      cxt->opts->buf = NULL;
      error("can't pack a %s", type2char(TYPEOF(dat)));
    }
  }
  UNPROTECT(unp);
  cxt->opts->as_is = as_is_sto;
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
    cxt->opts->buf = NULL;
    error("can't pack a singleton %s", type2char(TYPEOF(dat)));
  }
}

// generic for-loop macro
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
    cxt->opts->buf = NULL;
    error("Don't know how to pack a %s vector", type2char(TYPEOF(x)));
  }
}

// generic for-loop macro
#define PACK_NAMED_VECTOR(CXT, X, NAMES, ACCESSOR, STORE) {    \
    int len = LENGTH(X);                        \
    cw_pack_map_size(cxt, len);                 \
    for (int i = 0; i < len; i++) {             \
      pack_string(CXT, STRING_ELT(NAMES, i));   \
      STORE(CXT, ACCESSOR(X, i));               \
    }                                           \
  }

void pack_named_vector(cw_pack_context *cxt, SEXP x, SEXP names) {
  ASSERT(isVector(x));
  
  switch(TYPEOF(x)) {

  case RAWSXP:
    WARN_ONCE("Names discarded from raw object");
    pack_raw(cxt, x);
    break;
    
  case LGLSXP:
    PACK_NAMED_VECTOR(cxt, x, names, LOGICAL_ELT, pack_logical);
    break;
    
  case INTSXP:
    PACK_NAMED_VECTOR(cxt, x, names, INTEGER_ELT, pack_integer);
    break;

  case REALSXP:
    PACK_NAMED_VECTOR(cxt, x, names, REAL_ELT, pack_real);
    break;

  case VECSXP:
    PACK_NAMED_VECTOR(cxt, x, names, VECTOR_ELT, pack_sexp);
    break;

  case STRSXP:
    PACK_NAMED_VECTOR(cxt, x, names, STRING_ELT, pack_string);
    break;

  default:
    cxt->opts->buf = NULL;
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
  /* save bytes by packing integer if possible. */
  if (ISNA(x)) {
    cw_pack_nil(cxt);
  } else if (ceil(x) == x) {
    if (x >= 0 && x <= UINT32_MAX) {
      cw_pack_unsigned(cxt, x);
    } else if (x < 0 && x >= INT32_MIN) {
      cw_pack_signed(cxt, x);
    } else {
      /* NaN's end up here. */
      cw_pack_real(cxt, x);
    }
  } else {
    cw_pack_real(cxt, x);
  }
}

void pack_string(cw_pack_context *cxt, SEXP x) {
  assert_type(x, CHARSXP);
  if (x == NA_STRING) {
    cw_pack_nil(cxt);
  } else {
    if (getCharCE(x) != CE_UTF8) {
      /* reEnc allocates temp memory */
      void *vmax = vmaxget();
      const char *newbuf = reEnc(CHAR(x), getCharCE(x), CE_UTF8, 1);
      x = PROTECT(mkCharCE(newbuf, CE_UTF8));
      int len = R_nchar(x, Bytes, 0, 0, "");
      cw_pack_str(cxt, CHAR(x), len);
      UNPROTECT(1);
      /* free temp memory */
      vmaxset(vmax);
    } else {
      int len = R_nchar(x, Bytes, 0, 0, "");
      cw_pack_str(cxt, CHAR(x), len);
    }
  }
}

void pack_raw(cw_pack_context *cxt, SEXP x) {
  assert_type(x, RAWSXP);
  cw_pack_bin(cxt, RAW(x), LENGTH(x));
}
