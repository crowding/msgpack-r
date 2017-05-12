#include <inttypes.h>
#include "cwpack.h"
#include "vadr.h"
#include "utf8.h"

SEXP _unpackb(SEXP);

SEXP extract_value(cw_unpack_context *);
void cw_unpack_next_or_fail(cw_unpack_context *);
SEXP make_sexp_from_context(cw_unpack_context *);
SEXP extract_simplified_vector(cw_unpack_context *cxt);

SEXP fill_vector(cw_unpack_context *, SEXP, uint32_t, PROTECT_INDEX);

SEXP coerce(SEXP, SEXPTYPE);

const char *decode_return_code(int);
const char *decode_item_type(cwpack_item_types);

int check_string(const char *x, int len);

double i64_to_double(int64_t x);
double u64_to_double(uint64_t x);

SEXPTYPE type_to_sexptype(int t);

static int depth = 0;

#define LOG(FMT, ...)                                                   \
  Rprintf("%.*s %s: " FMT,                                              \
          MIN (depth, 40), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!",  \
          __FUNCTION__,                                                 \
          ## __VA_ARGS__)

SEXP _unpackb(SEXP dat) {
  assert_type(dat, RAWSXP);
  cw_unpack_context cxt;
  cw_unpack_context_init(&cxt, RAW(dat), LENGTH(dat), 0);

  depth = 0;
  
  SEXP out = extract_value(&cxt);
  return out;
}


SEXP extract_value(cw_unpack_context *cxt) {
  cw_unpack_next_or_fail(cxt);
  return make_sexp_from_context(cxt);
}


void cw_unpack_next_or_fail(cw_unpack_context *cxt) {
  cw_unpack_next(cxt);
  if (cxt->return_code != CWP_RC_OK) {
    error("Error encountered during unpacking: %s",
          decode_return_code(cxt->return_code));
  };
 }


SEXP make_sexp_from_context(cw_unpack_context *cxt) {
  LOG("Making sexp from a %s\n", decode_item_type(cxt->item.type));

  switch (cxt->item.type) {
  case CWP_ITEM_NIL:
    return R_NilValue;

  case CWP_ITEM_BOOLEAN:
    return ScalarLogical(cxt->item.as.boolean);

  case CWP_ITEM_ARRAY:
    return extract_simplified_vector(cxt);

  case CWP_ITEM_BIN:
    {
      uint32_t len = cxt->item.as.bin.length;
      SEXP out = PROTECT(allocVector(RAWSXP, len));
      memcpy(RAW(out), cxt->item.as.bin.start, len * sizeof(uint8_t));
      UNPROTECT(1);
      return out;
    }

  case CWP_ITEM_STR: {
    if (!check_string(cxt->item.as.str.start,
                      cxt->item.as.str.length)) {
      /* pack in a bin, (already warned) */
      uint32_t len = cxt->item.as.str.length;
      SEXP out = PROTECT(allocVector(RAWSXP, len));
      memcpy(RAW(out), cxt->item.as.str.start, len * sizeof(uint8_t));
      UNPROTECT(1);
      return out;
    } else {
      return ScalarString(mkCharLenCE(cxt->item.as.str.start,
                                      cxt->item.as.str.length,
                                       CE_UTF8));
    }
  }
    break;
    
  case CWP_ITEM_NEGATIVE_INTEGER:
  case CWP_ITEM_POSITIVE_INTEGER:
  case CWP_ITEM_FLOAT:
  case CWP_ITEM_DOUBLE: {
    SEXP buf;
    PROTECT_INDEX ix;

    PROTECT_WITH_INDEX(buf = allocVector(type_to_sexptype(cxt->item.type), 1),
                       &ix);
    buf = fill_vector(cxt, buf, 1, ix);
    UNPROTECT(1);
    return buf;
  }

  case CWP_ITEM_MAP:
    error("map not handled");

  case CWP_ITEM_EXT:
    error("ext not handled");

  case CWP_NOT_AN_ITEM:
    error("Non-item found in unpack");

  default:
    error("Unknown item type");
  }
}


SEXP _wtf() {
  int len = 2;

  SEXP buf;
  SEXPTYPE type = REALSXP;
  int ix;
  LOG("Allocating %s vector of length %d\n", type2char(type), len);
  PROTECT_WITH_INDEX(
                     buf = allocVector(type, len),
                     &ix);
  LOG("Protection index %d\n", ix);
    
  for(int i = 0; i < len; i++) {
    LOG("Filling item %d\n", i);
    REAL(buf)[i] = NA_REAL;
  }
  LOG("filled out a a %s\n", type2char(TYPEOF(buf)));
  LOG("That is, I filled out a a %s\n", type2char(TYPEOF(buf)));
  
  UNPROTECT(1);
  return buf;
}


SEXP extract_simplified_vector(cw_unpack_context *cxt) {
  ASSERT(cxt->item.type == CWP_ITEM_ARRAY);
  uint32_t len = cxt->item.as.array.size;
  LOG("Extracting array of %d elements, simplifying\n", len);
  SEXP buf;
  PROTECT_INDEX ix;

  /* Peek at the first item, allocate a buffer */
  if (len > 0) {
    cw_unpack_next_or_fail(cxt);
    LOG("first is a %s\n", decode_item_type(cxt->item.type));

    SEXPTYPE type = type_to_sexptype(cxt->item.type);
    LOG("Allocating %s vector of length %d\n", type2char(type), len);
    PROTECT_WITH_INDEX(
      buf = allocVector(type, len),
      &ix);
    LOG("Protection index %d\n", ix);
    
    buf = fill_vector(cxt, buf, len, ix);

  } else { /* len == 0 */
    PROTECT(buf = allocVector(LGLSXP, 0));
  }

  UNPROTECT(1);
  return buf;
}


SEXP coerce(SEXP buf, SEXPTYPE type) {
  LOG("Coercing %s to %s\n", type2char(TYPEOF(buf)), type2char(type));
  return coerceVector(buf, type);
}


SEXP fill_vector(cw_unpack_context *cxt, SEXP buf, uint32_t len, PROTECT_INDEX ix) {
  int i = 0;
  depth++;

  /* when called, we have an item loaded and ready for us in cxt. */

  int non_null_seen = 0;
  
  while(1) {
  UNPACK_VALUE:
    LOG("Packing item %d into a %s\n", i, type2char(TYPEOF(buf)));
    LOG("It's a %s\n", decode_item_type(cxt->item.type));

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
        REAL(buf)[i] = i64_to_double(cxt->item.as.i64);
        break;

      case CWP_ITEM_POSITIVE_INTEGER:
        REAL(buf)[i] = u64_to_double(cxt->item.as.u64);
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
        if (!check_string(cxt->item.as.str.start,
                          cxt->item.as.str.length)) {
          REPROTECT(buf = coerce(buf, VECSXP), ix);
          goto UNPACK_VALUE;
        } else {
          SET_STRING_ELT(buf,
                         i,
                         mkCharLen(cxt->item.as.str.start,
                                   cxt->item.as.str.length));
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
      error("Don't know how to fill out a %s (this should not happen)'");

    } /* switch(TYPEOF(buf)) */

    i++;
    if (i < len) {
      LOG("Unpacking next item\n");
      cw_unpack_next_or_fail(cxt);
    } else {
      break;
    };
  }
  LOG("Returning a %s\n", type2char(TYPEOF(buf)));
  depth--;
  return buf;
}


int check_string(const char *x, int len) {
  int non_ascii = 0;
  for (int i = 0; i < len; i++) {
    if (x[i] == 0) {
      WARN_ONCE("Embedded null in string, returning raw instead.");
      return 0;
    }
    if (x[i] & 0x80) {
      non_ascii = 1;
    }
  }

  /* maybe overly paranoid but I didn't see mkCharLenCE doing any verification */
  if (non_ascii) {
    if (!verify_utf8(x, len)) {
      WARN_ONCE("String is not valid UTF-8, returning raw instead.");
      return 0; 
    }
  }
  return 1;
}


double i64_to_double(int64_t x) {
  double xx = x;
  if ((int64_t)xx != x) {
    WARN_ONCE("Cast of integer %" PRId64 " to double loses precision", x);
  };
  return xx;
}

double u64_to_double(uint64_t x) {
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
  default: return VECSXP;
  }
}

const char *decode_return_code(int x) {
  switch(x) {
  case CWP_RC_OK: return "ok";
  case CWP_RC_END_OF_INPUT: return "end of input";
  case CWP_RC_BUFFER_OVERFLOW: return "buffer overflow";
  case CWP_RC_BUFFER_UNDERFLOW: return "buffer underflow";
  case CWP_RC_MALFORMED_INPUT: return "malformed input";
  case CWP_RC_WRONG_BYTE_ORDER: return "wrong byte order";
  case CWP_RC_ERROR_IN_HANDLER: return "error in handler";
  case CWP_RC_ILLEGAL_CALL: return "illegal call";
  case CWP_RC_MALLOC_ERROR: return "malloc error";
  case CWP_RC_STOPPED: return "stopped";
  default: return "unknown error";
  }
}

const char *decode_item_type(cwpack_item_types x) {
  switch(x) {
  case CWP_ITEM_MIN_RESERVED_EXT: return "CWP_ITEM_MIN_RESERVED_EXT";
  case CWP_ITEM_MAX_RESERVED_EXT: return "CWP_ITEM_MAX_RESERVED_EXT";
  case CWP_ITEM_MIN_USER_EXT: return "CWP_ITEM_MIN_USER_EXT";
  case CWP_ITEM_MAX_USER_EXT: return "CWP_ITEM_MAX_USER_EXT";
  case CWP_ITEM_NIL: return "CWP_ITEM_NIL";
  case CWP_ITEM_BOOLEAN: return "CWP_ITEM_BOOLEAN";
  case CWP_ITEM_POSITIVE_INTEGER: return "CWP_ITEM_POSITIVE_INTEGER";
  case CWP_ITEM_NEGATIVE_INTEGER: return "CWP_ITEM_NEGATIVE_INTEGER";
  case CWP_ITEM_FLOAT: return "CWP_ITEM_FLOAT";
  case CWP_ITEM_DOUBLE: return "CWP_ITEM_DOUBLE";
  case CWP_ITEM_STR: return "CWP_ITEM_STR";
  case CWP_ITEM_BIN: return "CWP_ITEM_BIN";
  case CWP_ITEM_ARRAY: return "CWP_ITEM_ARRAY";
  case CWP_ITEM_MAP: return "CWP_ITEM_MAP";
  case CWP_ITEM_EXT: return "CWP_ITEM_EXT";
  case CWP_NOT_AN_ITEM: return "CWP_NOT_AN_ITEM";
  default:
    if (x <= CWP_ITEM_MAX_USER_EXT
        && x >= CWP_ITEM_MIN_USER_EXT) {
      return "CWP_ITEM_USER_EXT(n)";
    } else if (x <= CWP_ITEM_MAX_RESERVED_EXT
               && x >= CWP_ITEM_MIN_RESERVED_EXT) {
      return "CWP_ITEM_MAX_USER_EXT(n)";
    } else {
      return "???";
    }
  }
}
