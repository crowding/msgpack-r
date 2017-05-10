#include <inttypes.h>
#include "cwpack.h"
#include "vadr.h"


SEXP _unpackb(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP extract_value(cw_unpack_context *);
void cw_unpack_next_or_fail(cw_unpack_context *);
SEXP make_sexp_from_context(cw_unpack_context *);
SEXP extract_simplified_vector(cw_unpack_context *cxt);

SEXP continue_as_na(
  cw_unpack_context *cxt, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);
SEXP coerce_and_continue(
  cw_unpack_context *cxt, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);
SEXP continue_as_logical(
  cw_unpack_context *, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);
SEXP continue_as_integer(
  cw_unpack_context *, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);
SEXP continue_as_list(
  cw_unpack_context *, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);
SEXP coerce_to_list_and_continue(
  cw_unpack_context *, SEXP, unsigned long, unsigned long, PROTECT_INDEX *);

const char *decode_return_code(int);
const char *decode_item_type(cwpack_item_types);

double i64_to_double(int64_t x);
double u64_to_double(uint64_t x);

SEXP _unpackb(SEXP dat, SEXP warn, SEXP use_envs, SEXP simplify, SEXP nil) {
  assert_type(dat, RAWSXP);
  cw_unpack_context cxt;
  cw_unpack_context_init(&cxt, RAW(dat), LENGTH(dat), 0);

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

/* TODO remove this duplication; just make a single-element vector and
  continue as vector */
SEXP make_sexp_from_context(cw_unpack_context *cxt) {
  LOG("Making sexp from a %s\n", decode_item_type(cxt->item.type));

  switch (cxt->item.type) {
  case CWP_ITEM_NIL:
    return R_NilValue;

  case CWP_ITEM_BOOLEAN:
    return ScalarLogical(cxt->item.as.boolean);

  case CWP_ITEM_NEGATIVE_INTEGER:
    if (cxt->item.as.i64 == NA_INTEGER
        || cxt->item.as.i64 < INT_MIN) {
      return ScalarReal(i64_to_double(cxt->item.as.u64));
    } else {
      return ScalarInteger(cxt->item.as.u64);
    }
    break;

  case CWP_ITEM_POSITIVE_INTEGER:
    if (cxt->item.as.u64 > UINT_MAX) {
      return ScalarReal(u64_to_double(cxt->item.as.u64));
    } else {
      return ScalarInteger(cxt->item.as.u64);
    }
    break;

  case CWP_ITEM_FLOAT:
    return ScalarReal(cxt->item.as.real);

  case CWP_ITEM_DOUBLE:
    return ScalarReal(cxt->item.as.long_real);

  case CWP_ITEM_STR:
    return ScalarString(mkCharLen(cxt->item.as.str.start, cxt->item.as.str.length));

  case CWP_ITEM_BIN:
    {
      uint32_t len = cxt->item.as.bin.length;
      SEXP out = PROTECT(allocVector(RAWSXP, len));
      memcpy(RAW(out), cxt->item.as.bin.start, len * sizeof(uint8_t));
      UNPROTECT(1);
      return out;
    }

  case CWP_ITEM_ARRAY:
    return extract_simplified_vector(cxt);

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



SEXP extract_simplified_vector(cw_unpack_context *cxt) {
  ASSERT2(cxt->item.type == CWP_ITEM_ARRAY, "extract_simplified_vector");
  /* start with the first item */
  int len = cxt->item.as.array.size;
  LOG("Extracting array of %d elements, simplifying\n", len);
  SEXP buf;
  if (len > 0) {
    PROTECT_INDEX ix;
    cw_unpack_next_or_fail(cxt);
    LOG("first is a %s\n", decode_item_type(cxt->item.type));

    switch(cxt->item.type) {

    case CWP_ITEM_NIL:
      PROTECT_WITH_INDEX(buf = allocVector(LGLSXP, len), &ix);
      buf = continue_as_na(cxt, buf, 0, len, &ix);
    case CWP_ITEM_BOOLEAN:
      PROTECT_WITH_INDEX(buf = allocVector(LGLSXP, len), &ix);
      buf = continue_as_logical(cxt, buf, 0, len, &ix);
      break;

    case CWP_ITEM_POSITIVE_INTEGER:
    case CWP_ITEM_NEGATIVE_INTEGER:
      PROTECT_WITH_INDEX(buf = allocVector(INTSXP, len), &ix);
      buf = continue_as_integer(cxt, buf, 0, len, &ix);
      break;

    default:
      PROTECT_WITH_INDEX(buf = allocVector(VECSXP, len), &ix);
      buf = continue_as_list(cxt, buf, 0, len, &ix);
    }
  } else { /* len == 0 */
    PROTECT(buf = allocVector(LGLSXP, 0));
  }

  UNPROTECT(1);
  return buf;
}


SEXP continue_as_na(cw_unpack_context *cxt,
                    SEXP buf,
                    unsigned long i,
                    unsigned long len,
                    PROTECT_INDEX *ix) {
  /* if we've only seen NA, we have no idea what type we'll end up with,
     so keep options open. */
  assert_type3(buf, LGLSXP, "continue_as_na");
  LOG("Continuing as NA (logical) vector\n");

  while (i < len) {
    LOG("Item %d is a %s\n", i, decode_item_type(cxt->item.type));
    switch(cxt->item.type) {

    case CWP_ITEM_NIL:
      LOGICAL(buf)[i] = NA_LOGICAL;
      break;

    case CWP_ITEM_BOOLEAN:
      buf = continue_as_logical(cxt, buf, i, len, ix);
      break;

    case CWP_ITEM_POSITIVE_INTEGER:
    case CWP_ITEM_NEGATIVE_INTEGER:
      buf = coerce_and_continue(cxt, buf, i, len, ix);
      break;

    default:
      buf = coerce_to_list_and_continue(cxt, buf, i, len, ix);
    }
    if (++i < len) cw_unpack_next_or_fail(cxt);
  }
  return buf;
}


SEXP continue_as_logical(cw_unpack_context *cxt,
                         SEXP buf,
                         unsigned long i,
                         unsigned long len,
                         PROTECT_INDEX *ix) {
  assert_type3(buf, LGLSXP, "continue_as_logical");
  LOG("Continuing as a logical vector\n");

  // when a "continue_*" is called, there is already an item in context
  // that needs to be handled. thus the ugly forloop spec:
  while(i < len) {
    LOG("Item %d is a %s\n", i, decode_item_type(cxt->item.type));

    switch(cxt->item.type) {

    case CWP_ITEM_NIL:
      LOGICAL(buf)[i] = NA_LOGICAL;
      break;

    case CWP_ITEM_BOOLEAN:
      LOGICAL(buf)[i] = (cxt->item.as.boolean ? TRUE : FALSE);
      break;

    default:
      buf = coerce_to_list_and_continue(cxt, buf, i, len, ix);
      break;
    }

    if (++i < len) cw_unpack_next_or_fail(cxt);
  }
  return buf;
}

SEXP continue_as_integer(cw_unpack_context *cxt,
                         SEXP buf,
                         unsigned long i,
                         unsigned long len,
                         PROTECT_INDEX *ix) {
  assert_type3(buf, INTSXP, "continue_as_integer");
  LOG("Continuing as an integer vector\n");

  while(i < len) {
    LOG("Item %d is a %s\n", i, decode_item_type(cxt->item.type));

    switch(cxt->item.type) {

    case CWP_ITEM_NIL:
      INTEGER(buf)[i] = NA_INTEGER;
      break;

    case CWP_ITEM_BOOLEAN:
      INTEGER(buf)[i] = (cxt->item.as.boolean ? 1 : 0);
      break;

    case CWP_ITEM_NEGATIVE_INTEGER:
      if (cxt->item.as.i64 == NA_INTEGER
          || cxt->item.as.i64 < INT_MIN) {
        buf = coerce_and_continue(cxt, buf, i, len, ix);
      } else {
        INTEGER(buf)[i] = (cxt->item.as.i64 ? 1 : 0);
      }
      break;

    case CWP_ITEM_POSITIVE_INTEGER:
      if (cxt->item.as.u64 > INT_MAX) {
        buf = coerce_and_continue(cxt, buf, i, len, ix);
      } else {
        INTEGER(buf)[i] = (cxt->item.as.u64 ? 1 : 0);
      }
      break;

    case CWP_ITEM_FLOAT:
      buf = coerce_and_continue(cxt, buf, 1, len, ix);
      break;

    default:
      buf = coerce_to_list_and_continue(cxt, buf, i, len, ix);
      break;
    }

    if (++i < len) cw_unpack_next_or_fail(cxt);
  }
  return buf;
}

SEXP coerce_and_continue(cw_unpack_context *cxt,
                            SEXP buf,
                            unsigned long i,
                            unsigned long len,
                            PROTECT_INDEX *ix) {
  error("need to implement coercion");
  return buf;
}


/* More genericity-via-macros */

#define MAP_TO_LIST(BUF, NEWBUF, ACCESS, NA, BOX, LENGTH) {   \
    for (int iii = 0; iii < LENGTH; iii++) {                  \
      if (ACCESS(BUF, iii) == NA)  {                          \
        LOG("Item %d is NA\n", iii);                          \
        SET_VECTOR_ELT(NEWBUF, iii, R_NilValue);              \
      } else {                                                \
        LOG("Item %d to " #BOX "\n", iii);                    \
        SET_VECTOR_ELT(NEWBUF, iii, BOX(ACCESS(BUF, iii)));   \
      }                                                       \
    }                                                         \
  }                                                           \


SEXP coerce_to_list_and_continue(cw_unpack_context *cxt,
                                 SEXP buf,
                                 unsigned long i,
                                 unsigned long len,
                                 PROTECT_INDEX *ix) {
  LOG("Coercing to list from %s\n", type2char(TYPEOF(buf)));
  SEXP newbuf = PROTECT(allocVector(VECSXP, LENGTH(buf)));
  switch(TYPEOF(buf)) {

  case LGLSXP:
    MAP_TO_LIST(buf, newbuf, LOGICAL_ELT, NA_LOGICAL, ScalarLogical, i);
    break;

  case INTSXP:
    MAP_TO_LIST(buf, newbuf, INTEGER_ELT, NA_INTEGER, ScalarInteger, i);
    break;

  case REALSXP:
    MAP_TO_LIST(buf, newbuf, REAL_ELT, NA_REAL, ScalarReal, i);
    break;

  case STRSXP:
    MAP_TO_LIST(buf, newbuf, STRING_ELT, NA_STRING, ScalarString, i);
    break;

  default:
    error("Don't know how to turn a %s into a list, at coerce_to_list_and_continue",
          type2char(TYPEOF(buf)));
  }

  return continue_as_list(cxt, newbuf, i, len, ix);
}


SEXP continue_as_list(cw_unpack_context *cxt,
                      SEXP buf,
                      unsigned long i,
                      unsigned long len,
                      PROTECT_INDEX *ix) {
  assert_type3(buf, VECSXP, "continue_as_list");
  LOG("Continuing as list\n");

  while (i < len) {
    SET_VECTOR_ELT(buf, i, make_sexp_from_context(cxt));

    if (++i < len) {
      cw_unpack_next_or_fail(cxt);
    }
  }
  return buf;
}

double i64_to_double(int64_t x) {
  double xx = x;
  if ((int64_t)xx != x) {
    WARN_ONCE("Cast of integer %" PRId64 " to double loses precision", x);
  }
  return xx;
}

double u64_to_double(uint64_t x) {
  double xx = x;
  if ((uint64_t)xx != x) {
    WARN_ONCE("Cast of integer %" PRIu64 " to double loses precision", x);
  }
  return xx;
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
  default: return "???";
  }
}
