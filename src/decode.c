#include "decode.h"

#include <inttypes.h>
#include "utf8.h"

#ifdef DEBUG
#define LOGD(FMT, ...)                                   \
  LOG("%.*s " FMT,                                       \
      MIN (cxt->opts->depth, 40),                        \
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!",        \
      ##__VA_ARGS__);
#else
#define LOGD(...) NULL
#endif

SEXP extract_sexp(cw_unpack_context *);
int underflow_handler(cw_unpack_context *, unsigned long);
void cw_unpack_next_or_fail(cw_unpack_context *);
SEXP make_sexp_from_context(cw_unpack_context *);

SEXP extract_simplified_vector(cw_unpack_context *);
SEXP fill_vector(cw_unpack_context *, SEXP, uint32_t, PROTECT_INDEX, SEXP);
SEXP coerce(SEXP, SEXPTYPE);

SEXP extract_env(cw_unpack_context *);
SEXP new_env(SEXP parent);

SEXP make_charsxp_or_null(cw_unpack_context *);
SEXP make_charsxp_or_raw_from_context(cw_unpack_context *);
SEXP make_raw_from_context(cw_unpack_context *);
SEXP make_ext_from_context(cw_unpack_context *);

SEXP always_make_charsxp_from_context(cw_unpack_context *);

double i64_to_double(cw_unpack_context *, int64_t);
double u64_to_double(cw_unpack_context *, uint64_t);

SEXPTYPE type_to_sexptype(int t);

const char *decode_item_type(cwpack_item_types);

static long calls = 0;

int isNA (SEXP item) {
  if (asReal(item) == NA_REAL) return 1;
  else return 0;
}

SEXP _unpack_opts(SEXP dict,
                  SEXP use_df,
                  SEXP simplify,
                  SEXP package,
                  SEXP max_size,
                  SEXP max_depth,
                  SEXP underflow_handler) {
  SEXP optsxp = PROTECT(allocVector(RAWSXP, sizeof(unpack_opts)));
  unpack_opts *opts = (unpack_opts*)(RAW(optsxp));
  opts->use_df = asLogical(use_df);
  opts->dict = dict;
  opts->simplify = asLogical(simplify);
  opts->package = package;
  opts->underflow_handler = underflow_handler;

  if (isNA(max_size)) opts->max_pending = ULONG_MAX;
  else opts->max_pending = asReal(max_size);
  if (isNA(max_depth)) opts->max_depth = UINT_MAX;
  else opts->max_depth = asInteger(max_depth);

  /* To make sure that R does not rinalize the objects pointed to by
     this struct, hang the object references off of it.
     
     TODO: return an external pointer instead of the RAWSXP. */
  setAttrib(optsxp, install("refs"), CONS(dict,
                                          CONS(package,
                                               CONS(underflow_handler,
                                                    R_NilValue))));
  UNPROTECT(1);
  return optsxp;
}


int init_unpack_context(cw_unpack_context *cxt,
                        SEXP optsxp,
                        SEXP dat,
                        unsigned long start) {
  assert_type(dat, RAWSXP);
  assert_type(optsxp, RAWSXP);
  ASSERT(LENGTH(optsxp) == sizeof(unpack_opts));
  ASSERT(start <= LENGTH(dat))
  cw_unpack_context_init(cxt, RAW(dat) + start, LENGTH(dat) - start, 0);
  cxt->opts = (unpack_opts *)RAW(optsxp);

  cxt->opts->depth = 0;
  cxt->opts->pending = 0;
  return 0;
}


SEXP _unpack_msg(SEXP dat, SEXP opts) {
  calls++;
  assert_type(dat, RAWSXP);
  cw_unpack_context cxt;
  int protections = init_unpack_context(&cxt, opts, dat, 0);
  LOG("depth = %u, pending = %lu", cxt.opts->depth, cxt.opts->pending); 
  SEXP out = extract_sexp(&cxt);
  LOG("depth = %u, pending = %lu", cxt.opts->depth, cxt.opts->pending); 
  ASSERT(cxt.opts->depth == 0);
  ASSERT(cxt.opts->pending == 0);
  UNPROTECT(protections);
  return out;
}

int handle_unpack_underflow(cw_unpack_context *cxt, unsigned long x) {
  /* bounce back to R to read another buffer */
  if (cxt->opts->underflow_handler == R_NilValue)
    error("No underflow handler provided\n");
  assert_type3(cxt->opts->underflow_handler, CLOSXP, "Underflow handler was not a closure");
  error("Actually I don't know how to handle underflow");
  return 1;
};

SEXP _unpack_msg_partial(SEXP buf, SEXP start, SEXP opts) {
  calls++;
  cw_unpack_context cxt;
  LOG("buf is %x", RAW(buf));
  LOG("start is %x", asInteger(start));
  int protections = init_unpack_context(&cxt, opts, buf, asInteger(start));
  LOG("status is %s", decode_return_code(cxt.return_code));
  LOG("cxt.start   is %x", cxt.start);
  LOG("cxt.current is %x", cxt.current);
  SEXP msg = PROTECT(extract_sexp(&cxt));
  protections++;
  LOG("status is now %s", decode_return_code(cxt.return_code));
  LOG("cxt.start   now %x", cxt.start);
  LOG("cxt.current now %x", cxt.current);
  
  SEXP out = list3(msg,
                   ScalarInteger(cxt.current - RAW(buf)),
                   ScalarString(mkChar(decode_return_code(cxt.return_code))));
  
  ASSERT(cxt.opts->depth == 0);
  ASSERT(cxt.opts->pending == 0);
  UNPROTECT(protections);
  return out;
}


SEXP extract_sexp(cw_unpack_context *cxt) {
  cw_unpack_next_or_fail(cxt);
  return make_sexp_from_context(cxt);
}


void cw_unpack_next_or_fail(cw_unpack_context *cxt) {
  cw_unpack_next(cxt);
  if (cxt->return_code != CWP_RC_OK) {
    error("%s",
          decode_return_code(cxt->return_code));
  }
}


void add_pending(cw_unpack_context *cxt, unsigned long howmany) {
  if (cxt->opts->depth >= cxt->opts->max_depth) {
    error("Message too deeply nested");
  }
  cxt->opts->pending += howmany;
  if (cxt->opts->pending >= cxt->opts->max_pending) {
    error("Pending message too long");
  }
  LOGD("depth = %u, pending = %lu", cxt->opts->depth, cxt->opts->pending); 
}


SEXP alloc_vector(cw_unpack_context *cxt, SEXPTYPE type, unsigned long len) {
  add_pending(cxt, len);
  return allocVector(type, len);
}


SEXP make_sexp_from_context(cw_unpack_context *cxt) {
  LOGD("Making sexp from a %s", decode_item_type(cxt->item.type));
  cwpack_item_types x = cxt->item.type;
  switch (x) {
  case CWP_ITEM_NIL:
    return ScalarLogical(NA_LOGICAL);

  case CWP_ITEM_BOOLEAN:
    return ScalarLogical(cxt->item.as.boolean);

  case CWP_ITEM_ARRAY:
    return extract_simplified_vector(cxt);

  case CWP_ITEM_BIN:
    return make_raw_from_context(cxt);

  case CWP_ITEM_STR: {
    SEXP x = PROTECT(make_charsxp_or_raw_from_context(cxt));
    LOGD("x is a %s", type2char(TYPEOF(x)));
    if (TYPEOF(x) == CHARSXP) {
      UNPROTECT(1);
      return ScalarString(x);
    } else {
      UNPROTECT(1);
      return x;
    }
  }

  case CWP_ITEM_NEGATIVE_INTEGER:
  case CWP_ITEM_POSITIVE_INTEGER:
  case CWP_ITEM_FLOAT:
  case CWP_ITEM_DOUBLE: {
    SEXP buf;
    PROTECT_INDEX ix;
    PROTECT_WITH_INDEX(buf = alloc_vector(cxt, type_to_sexptype(cxt->item.type), 1),
                       &ix);
    buf = fill_vector(cxt, buf, 1, ix, R_NilValue);
    UNPROTECT(1);
    return buf;
  }

  case CWP_ITEM_MAP:
    if (TYPEOF(cxt->opts->dict) == ENVSXP) {
      return extract_env(cxt);
    } else {
      return extract_simplified_vector(cxt);
    }

  default:
    if (x <= CWP_ITEM_MAX_USER_EXT
        && x >= CWP_ITEM_MIN_USER_EXT) {
      return make_ext_from_context(cxt);
    } else if (x <= CWP_ITEM_MAX_RESERVED_EXT
               && x >= CWP_ITEM_MIN_RESERVED_EXT) {
      return make_ext_from_context(cxt);
    } else {
      error("Unsupported item: %s", decode_item_type(cxt->item.type));
    }
  }
}


SEXP extract_simplified_vector(cw_unpack_context *cxt) {
  int has_names = 0;
  switch(cxt->item.type) {
  case CWP_ITEM_ARRAY:
    has_names = 0; break;
  case CWP_ITEM_MAP:
    has_names = 1; break;
  default:
    error("Can't extract vector from a %s", decode_item_type(cxt->item.type));
  }

  uint32_t len = cxt->item.as.array.size;
  LOGD("Extracting array of %d elements, simplifying", len);
  SEXP buf;
  PROTECT_INDEX ix;
  int protections = 0;

  SEXP names = R_NilValue;
  if(has_names) {
    LOGD("Allocating names[%d]", len);
    PROTECT(names = alloc_vector(cxt, STRSXP, len));
    protections++;
  }

  /* Peek at the first item, allocate a buffer */
  if (len > 0) {
    if(has_names) {
      cw_unpack_next_or_fail(cxt);
      SEXP name = PROTECT(always_make_charsxp_from_context(cxt));
      SET_STRING_ELT(names, 0, name);
      UNPROTECT(1);
    }
    cw_unpack_next_or_fail(cxt);
    LOGD("first is a %s", decode_item_type(cxt->item.type));

    SEXPTYPE type;
    if (cxt->opts->simplify) {
      type = type_to_sexptype(cxt->item.type);
    } else {
      type = VECSXP;
    }
    PROTECT_WITH_INDEX(buf = alloc_vector(cxt, type, len),
                       &ix);
    protections++;
    LOGD("Allocated %s vector of length %d, protection id %d",
         type2char(type), len, ix);

    buf = fill_vector(cxt, buf, len, ix, names);

  } else { /* len == 0 */
    buf = allocVector(LGLSXP, 0);
  }

  UNPROTECT(protections);
  return buf;
}


SEXP fill_vector(cw_unpack_context *cxt, SEXP buf, uint32_t len,
                 PROTECT_INDEX ix, SEXP names) {
  cxt->opts->depth++;

  /* when called, we have the first item "primed" for us in cxt. Also
     buf has been protected. */

  int non_null_seen = 0;
  int output_data_frame = ((names != R_NilValue)
                           && cxt->opts->use_df
                           && cxt->item.type == CWP_ITEM_ARRAY
                           && len > 0);
  long df_rows = 0;
  if (output_data_frame) {
    LOGD("Might be a data frame");
    df_rows = cxt->item.as.array.size;
  }

  for (int i = 0; i < len; i++) {                    /* loop once per item read */
    if (i > 0) {        /* skip what we did when we peeked at first item */
      if (names != R_NilValue) {
        LOGD("Reading name %d", i);
        cw_unpack_next_or_fail(cxt);
        SEXP c = PROTECT(always_make_charsxp_from_context(cxt));
        SET_STRING_ELT(names, i, c);
        UNPROTECT(1);
      }

      LOGD("Unpacking item %d", i);
      cw_unpack_next_or_fail(cxt);

      if (output_data_frame
          && (cxt->item.type != CWP_ITEM_ARRAY
              || cxt->item.as.array.size != df_rows)) {
        LOGD("Not a data frame after all");
        output_data_frame = 0;
      }
    }

    cxt->opts->pending -= (names == R_NilValue ? 1 : 2);
    
  UNPACK_VALUE:                 /* goto here to "try again" after coercion */
    LOGD("Storing item %d, a %s, into a %s",
         i, decode_item_type(cxt->item.type), type2char(TYPEOF(buf)));

    switch (TYPEOF(buf)) {

    case LGLSXP:

      switch(cxt->item.type) {

      case CWP_ITEM_NIL:
        LOGICAL(buf)[i] = NA_LOGICAL;
        break;

      case CWP_ITEM_BOOLEAN:
        non_null_seen = 1;
        LOGICAL(buf)[i] = (cxt->item.as.boolean ? TRUE : FALSE);
        break;

      default:
        if (non_null_seen) {
          REPROTECT(buf = coerce(buf, VECSXP), ix);
        } else {
          non_null_seen = 1;
          REPROTECT(buf = coerce(buf, type_to_sexptype(cxt->item.type)), ix);
        }
        goto UNPACK_VALUE;
      }
      break;


    case INTSXP:

      switch(cxt->item.type) {

      case CWP_ITEM_NIL:
        INTEGER(buf)[i] = NA_INTEGER;
        break;


      case CWP_ITEM_NEGATIVE_INTEGER:
        if (cxt->item.as.i64 == NA_INTEGER
            || cxt->item.as.i64 < INT_MIN) {
          REPROTECT(buf = coerce(buf, REALSXP), ix);
          goto UNPACK_VALUE;
        } else {
          INTEGER(buf)[i] = cxt->item.as.i64;
        }
        break;

      case CWP_ITEM_POSITIVE_INTEGER:
        if (cxt->item.as.u64 > INT_MAX) {
          REPROTECT(buf = coerce(buf, REALSXP), ix);
          goto UNPACK_VALUE;
        } else {
          INTEGER(buf)[i] = cxt->item.as.u64;
        }
        break;

      case CWP_ITEM_FLOAT:
      case CWP_ITEM_DOUBLE:
        REPROTECT(buf = coerce(buf, REALSXP), ix);
        goto UNPACK_VALUE;

      default:
        REPROTECT(buf = coerce(buf, VECSXP), ix);
        goto UNPACK_VALUE;
      }

      break;


    case REALSXP:
      switch(cxt->item.type) {

      case CWP_ITEM_NIL:
        REAL(buf)[i] = NA_REAL;
        break;

      case CWP_ITEM_NEGATIVE_INTEGER:
        REAL(buf)[i] = i64_to_double(cxt, cxt->item.as.i64);
        break;

      case CWP_ITEM_POSITIVE_INTEGER:
        REAL(buf)[i] = u64_to_double(cxt, cxt->item.as.u64);
        break;

      case CWP_ITEM_FLOAT:
        REAL(buf)[i] = cxt->item.as.real;
        break;

      case CWP_ITEM_DOUBLE:
        REAL(buf)[i] = cxt->item.as.long_real;
        break;

      default:
        REPROTECT(buf = coerce(buf, VECSXP), ix);
        goto UNPACK_VALUE;
      }

      break;


    case STRSXP:
      switch(cxt->item.type) {

      case CWP_ITEM_NIL:
        SET_STRING_ELT(buf, i, NA_STRING);
        break;

      case CWP_ITEM_STR:
        {
          SEXP s = PROTECT(make_charsxp_or_null(cxt));
          if (s == R_NilValue) {
            REPROTECT(buf = coerce(buf, VECSXP), ix);
            UNPROTECT(1);
            goto UNPACK_VALUE;
          } else {
            SET_STRING_ELT(buf, i, s);
          }
          UNPROTECT(1);
        }
        break;

      default:
        REPROTECT(buf = coerce(buf, VECSXP), ix);
        goto UNPACK_VALUE;
      }

      break;


    case VECSXP:
      SET_VECTOR_ELT(buf, i, make_sexp_from_context(cxt));

      break;


    default:
      error("Don't know how to fill out a %s (this shouldn't happen)",
            type2char(TYPEOF(buf)));

    } /* end switch(TYPEOF(buf)) */

  } /* end for(i = ... */

  if (names != R_NilValue) {
    setAttrib(buf, R_NamesSymbol, names);
  }

  if (output_data_frame) {
    LOGD("Making a data frame");
    SEXP call = PROTECT(lang2(install(".set_row_names"), ScalarInteger(df_rows)));
    SEXP rn = PROTECT(eval(call, R_BaseEnv));
    setAttrib(buf, R_RowNamesSymbol, rn);
    setAttrib(buf, R_ClassSymbol, ScalarString(mkChar("data.frame")));
    UNPROTECT(2);
  }

  LOGD("Returning a %s", type2char(TYPEOF(buf)));
  cxt->opts->depth--;
  return buf;
}


SEXP coerce(SEXP buf, SEXPTYPE type) {
  LOG("Coercing %s to %s", type2char(TYPEOF(buf)), type2char(type));
  return coerceVector(buf, type);
}


SEXP extract_env(cw_unpack_context *cxt) {
  ASSERT(cxt->item.type == CWP_ITEM_MAP);
  ASSERT(TYPEOF(cxt->opts->dict) == ENVSXP);
  int nkeys = cxt->item.as.map.size;
  add_pending(cxt, 2*nkeys);
  SEXP env = PROTECT(new_env(cxt->opts->dict));
  for (int i = 0; i < nkeys; i++) {
    cw_unpack_next_or_fail(cxt);
    cxt->opts->pending--;
    SEXP key = PROTECT(always_make_charsxp_from_context(cxt));
    SEXP value = PROTECT(extract_sexp(cxt));
    cxt->opts->pending--;
    if (LENGTH(key) == 0) {
      WARN_ONCE("Item with empty name was discarded");
    } else {
      SEXP sym = PROTECT(installChar(key));

      if (sym == R_MissingArg || sym == R_DotsSymbol || DDVAL(sym)) {
        WARN_ONCE("Item with key `%s` was discarded", CHAR(PRINTNAME(sym)));
      } else {
        LOGD("storing variable `%s` into a %s",
             CHAR(PRINTNAME(sym)), type2char(TYPEOF(env)));
        defineVar(sym, value, env);
      }
      UNPROTECT(1);
    }
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return env;
}


SEXP new_env(SEXP parent) {
  ASSERT(TYPEOF(parent) == ENVSXP);
  SEXP call = PROTECT(lang3(install("new.env"), ScalarLogical(TRUE), parent));
  SEXP x = eval(call, R_BaseEnv);
  UNPROTECT(1);
  return x;
}


SEXP make_charsxp_or_raw_from_context(cw_unpack_context *cxt) {
  ASSERT(cxt->item.type == CWP_ITEM_STR || cxt->item.type == CWP_ITEM_BIN);
  SEXP c = PROTECT(make_charsxp_or_null(cxt));
  if (c == R_NilValue) {
    UNPROTECT(1);
    return make_raw_from_context(cxt);
  } else {
    UNPROTECT(1);
    return c;
  }
}


SEXP make_raw_from_context(cw_unpack_context *cxt) {
  uint32_t len = cxt->item.as.str.length;
  SEXP out = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(out), cxt->item.as.str.start, len * sizeof(uint8_t));
  UNPROTECT(1);
  return out;
}


SEXP make_ext_from_context(cw_unpack_context *cxt) {
  char typename[32];
  WARN_ONCE("Extension type %d decoded as raw", cxt->item.type);
  snprintf(typename, sizeof(typename)/sizeof(char), "ext%d", cxt->item.type);
  SEXP out = PROTECT(make_raw_from_context(cxt));
  setAttrib(out, R_ClassSymbol, ScalarString(mkChar(typename)));
  UNPROTECT(1);
  return out;
}


SEXP make_charsxp_or_null(cw_unpack_context *cxt) {
  if (cxt->item.type == CWP_ITEM_STR || cxt->item.type == CWP_ITEM_BIN) {
    const char *buf = cxt->item.as.str.start;
    int len = cxt->item.as.str.length;
    int non_ascii = 0;

    for (int i = 0; i < len; i++) {
      if (buf[i] == 0) {
        WARN_ONCE("Embedded null in string, returning raw instead.");
        return R_NilValue;
      }
      if (buf[i] & 0x80) {
        non_ascii = 1;
      }
    }

    if (non_ascii) {
      /* maybe overly paranoid but I didn't see mkCharLenCE doing such
         verification */
      if (!verify_utf8(buf, len)) {
        WARN_ONCE("String is not valid UTF-8, returning raw instead.");
        return R_NilValue;
      }
    }

    return mkCharLenCE(buf, len, CE_UTF8);
  } else if (cxt->item.type == CWP_ITEM_NIL) {
    return NA_STRING;
  } else {
    return R_NilValue;
  }
}


SEXP always_make_charsxp_from_context(cw_unpack_context *cxt) {
  SEXP x = PROTECT(make_charsxp_or_null(cxt));
  if (x == R_NilValue) {
    WARN_ONCE("Non-string used as key was coerced to string");
    SEXP item = PROTECT(make_sexp_from_context(cxt));
    SEXP call = PROTECT(lang2(install("repr"), item));
    SEXP convert = PROTECT(eval(call, cxt->opts->package));
    LOG("repr returned a %s", type2char(TYPEOF(convert)));
    SEXP chr = STRING_ELT(convert, 0);
    UNPROTECT(4);
    return chr;
  } else {
    UNPROTECT(1);
    return x;
  }
}


double i64_to_double(cw_unpack_context *cxt, int64_t x) {
  double xx = x;
  if ((int64_t)xx != x) {
    WARN_ONCE("Cast of integer %" PRId64 " to double loses precision", x);
  };
  return xx;
}


double u64_to_double(cw_unpack_context *cxt, uint64_t x) {
  double xx = x;
  if ((uint64_t)xx != x) {
    WARN_ONCE("Cast of integer %" PRIu64 " to double loses precision", x);
  }
  return xx;
}


SEXPTYPE type_to_sexptype(int t) {
  switch(t) {
  case CWP_ITEM_NIL:
  case CWP_ITEM_BOOLEAN: return LGLSXP;
  case CWP_ITEM_POSITIVE_INTEGER:
  case CWP_ITEM_NEGATIVE_INTEGER: return INTSXP;
  case CWP_ITEM_FLOAT:
  case CWP_ITEM_DOUBLE: return REALSXP;
  case CWP_ITEM_STR: return STRSXP;
  default:
    {
      if (t <= CWP_ITEM_MAX_USER_EXT
          && t >= CWP_ITEM_MIN_USER_EXT) {
        return RAWSXP;
      } else if (t <= CWP_ITEM_MAX_RESERVED_EXT
                 && t >= CWP_ITEM_MIN_RESERVED_EXT) {
        return RAWSXP;
      } else {
        return VECSXP;
      }
    }
  }
}


const char *decode_item_type(cwpack_item_types x) {
  switch(x) {
  case CWP_ITEM_NIL: return "nil";
  case CWP_ITEM_BOOLEAN: return "boolean";
  case CWP_ITEM_POSITIVE_INTEGER: return "positive integer";
  case CWP_ITEM_NEGATIVE_INTEGER: return "negative integer";
  case CWP_ITEM_FLOAT: return "float";
  case CWP_ITEM_DOUBLE: return "double";
  case CWP_ITEM_STR: return "string";
  case CWP_ITEM_BIN: return "binary";
  case CWP_ITEM_ARRAY: return "array";
  case CWP_ITEM_MAP: return "map";
  case CWP_NOT_AN_ITEM: return "not an item";
  default:
    if (x <= CWP_ITEM_MAX_USER_EXT
        && x >= CWP_ITEM_MIN_USER_EXT) {
      return "CWP_ITEM_USER_EXT(n)";
    } else if (x <= CWP_ITEM_MAX_RESERVED_EXT
               && x >= CWP_ITEM_MIN_RESERVED_EXT) {
      return "CWP_ITEM_RESERVED_EXT(n)";
    } else {
      return "???";
    }
  }
}
