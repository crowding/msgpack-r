#include "cwpack.h"
#include "vadr.h"

typedef struct unpack_options {
  unsigned int warn:1;
  unsigned int compatible:1;
  unsigned int use_envs: 1;
  unsigned int use_data_frames: 1;
} unpack_options;

SEXP _unpackb(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP extract_value(cw_unpack_context *);

const char *decode_cwp_error(int x);


SEXP _unpackb(SEXP dat, SEXP warn, SEXP use_envs, SEXP simplify, SEXP nil) {
  assert_type(dat, RAWSXP);
  cw_unpack_context cxt;
  cw_unpack_context_init(&cxt, RAW(dat), LENGTH(dat), 0);

  SEXP out = extract_value(&cxt);
  
  return out;
}


SEXP extract_value(cw_unpack_context *cxt) {
  cw_unpack_next(cxt);
  if (cxt->return_code != CWP_RC_OK) {
    error("Error encountered during unpacking: %s",
          decode_cwp_error(cxt->return_code));
  };

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
    {
      uint32_t len = cxt->item.as.bin.length;
      SEXP out = PROTECT(allocVector(RAWSXP, len));
      memcpy(RAW(out), cxt->item.as.bin.start, len * sizeof(uint8_t));
      UNPROTECT(1);
      return out;
    }

  case CWP_ITEM_ARRAY:
    {
      uint32_t len = cxt->item.as.array.size;
      SEXP out = PROTECT(allocVector(VECSXP, len));
      for (int i = 0; i < len; i++) {
        SEXP val = PROTECT(extract_value(cxt));
        SET_VECTOR_ELT(out, i, val);
      }
      UNPROTECT(1);
      return out;
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


const char *decode_cwp_error(int x) {
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
