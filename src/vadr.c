#include "vadr.h"

#include "cwpack.h"

/* Assert that some object is a type. */
void assert_type(SEXP x, SEXPTYPE type) {
  if (TYPEOF(x) != type) {
    error("Expected %s, got %s", type2char(type), type2char(TYPEOF(x)));
  }
}

/* As above, with extra words about what thing is to be an expected type */
void assert_type3(SEXP x, SEXPTYPE type, const char *what) {
  if (TYPEOF(x) != type) {
    error("Expected %s in %s, got %s",
          type2char(type), what, type2char(TYPEOF(x)));
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
