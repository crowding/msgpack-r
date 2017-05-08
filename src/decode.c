#include "cwpack.h"
#include <R.h>
#include <Rinternals.h>

void assert_type(SEXP, SEXPTYPE);

void assert_type(SEXP x, SEXPTYPE type) {
  if (TYPEOF(x) != type) {
    error("Expected %s, got %s", type2char(type), type2char(TYPEOF(x)));
  }
}

int underflow_handler(cw_unpack_context *cxt, unsigned long wanted) {
  error("Underflow (need %d more bytes)", wanted);
}

SEXP extract_value(cw_unpack_context *);

SEXP _unpackb(SEXP dat, SEXP warn, SEXP use_envs, SEXP simplify, SEXP nil) {
  assert_type(dat, RAWSXP);
  cw_unpack_context cxt;
  cw_unpack_context_init(&cxt, RAW(dat), LENGTH(dat), &underflow_handler);

  SEXP out = extract_value(&cxt);
  return out;
}

SEXP extract_value(cw_unpack_context *cxt) {
  cw_unpack_next(cxt);
  if (cxt->return_code) error("cw_unpack error code %d", cxt->return_code);

  switch (cxt->item.type) {
  case CWP_ITEM_NIL:
    return R_NilValue;
    
  case CWP_ITEM_BOOLEAN:
    return ScalarLogical(cxt->item.as.boolean);

  case CWP_ITEM_POSITIVE_INTEGER:
    return ScalarInteger(cxt->item.as.u64);

  case CWP_ITEM_NEGATIVE_INTEGER:
    return ScalarInteger(cxt->item.as.i64);

  case CWP_ITEM_FLOAT:
    return ScalarReal(cxt->item.as.real);

  case CWP_ITEM_DOUBLE:
    return ScalarReal(cxt->item.as.long_real);

  case CWP_ITEM_STR:
    return ScalarString(mkCharLen(cxt->item.as.str.start, cxt->item.as.str.length));

  case CWP_ITEM_BIN:
    error("bin not handled");

  case CWP_ITEM_ARRAY:
    error("array not handled");

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
