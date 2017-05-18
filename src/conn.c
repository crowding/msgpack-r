#include <R.h>
#include <RInternals.h>
#include "vadr.h"
#include "decode.h"
#include "encode.h"
#include "cwpack.h"

/* Premature optimization. Do not continue until you've profiled time
   spent copying with R level implementation. */

int flush_buffer(cw_pack_context *cxt, unsigned long more);
int fill_buffer(cw_unpack_context *cxt, unsigned long more);

int init_write_context(cw_pack_context *cxt, SEXP opts, SEXP conn) {
  ASSERT(TYPEOF(opts) == RAWSXP && LENGTH(opts) == sizeof(pack_opts));
  cxt->opts = (pack_opts *) RAW(opts);

  cxt->opts->conn = conn;

  /* allocate a buffer */
  PROTECT_WITH_INDEX(cxt->opts->buf = allocVector(RAWSXP, cxt->opts->buf_size),
                     &cxt->opts->buf_index);

  cw_pack_context_init(cxt,
                       RAW(cxt->opts->buf),
                       LENGTH(cxt->opts->buf),
                       &flush_buffer);
  cw_pack_set_compatibility(cxt, cxt->opts->compatible);
  return 1;
}

SEXP _write_msgs(SEXP conn, SEXP messages, SEXP opts) {
  cw_pack_context cxt;
  assert_type(messages, LISTSXP);
  
  int protections = init_write_context(&cxt, opts, conn);

  for (int i = 0; i < LENGTH(messages); i++) {
    pack_sexp(&cxt, VECTOR_ELT(messages, i));
  }
  flush_buffer(&cxt, 0);
  
  if (cxt.return_code != CWP_RC_OK) {
    error("%s", decode_return_code(cxt.return_code));
  }

  UNPROTECT(protections);
  return R_NilValue;
}

int flush_buffer(cw_pack_context *cxt, unsigned long more) {
  /* hand the present RAWSXP off to the connection */
  unsigned long length = (cxt->current - cxt->start) / sizeof(uint8_t);
  SETLENGTH(cxt->opts->buf, length);
  if (LENGTH > 0) {
    SEXP call = PROTECT(lang3(install("writeBin"), cxt->opts->conn, cxt->opts->buf));
    eval(call, R_BaseEnv);
    UNPROTECT(1);
  }

  /* allocate a new RAWSXP */
  REPROTECT(cxt->opts->buf = allocVector(RAWSXP, cxt->opts->buf_size), cxt->opts->buf_index);
  cxt->start = RAW(cxt->opts->buf);
  cxt->end = RAW(cxt->opts->buf) + cxt->opts->buf_size;
  cxt->current = cxt->start;
  return length;
}


int init_read_context(cw_unpack_context *cxt, SEXP opts, SEXP conn) {
  ASSERT(TYPEOF(opts) == RAWSXP && LENGTH(opts) == sizeof(unpack_opts));
  cxt->opts = (unpack_opts *) RAW(opts);
  cxt->opts->conn = conn;
  PROTECT_WITH_INDEX(cxt->opts->buf = allocVector(RAWSXP, 0), &cxt->opts->buf_index);
  cw_unpack_context_init(cxt, RAW(cxt->opts->buf), LENGTH(cxt->opts->buf), &fill_buffer);
  cxt->opts->msg_start = cxt->current - cxt->start;
  return 1;
}

SEXP _read_msgs(SEXP conn, SEXP opts) {
  /* If we are reading in a non-blocking manner, we will reach the end
     of the stream with a message partially read. So as I consumer a
     number of RAWSXPS that might conpose an object, I will need to
     push down a stack */

  cw_unpack_context cxt;
  int protections = init_read_context(&cxt, opts, conn);
  error("implementation needed");
  UNPROTECT(protections);
}

int fill_buffer(cw_unpack_context *cxt, unsigned long more) {
  ASSERT(cxt->current == cxt->end);
  SEXP call = PROTECT(lang4(install("readBin"),
                              cxt->opts->conn,
                              cxt->opts->buf,
                              cxt->opts->read_size)) ;

  /* when reading non-blocking, we may have to refill the buffer several times
     per item, only to need to save our work.
     Therefore need to remember our previous buffers until the read loop clears an item.*/
  SEXP newbuf = PROTECT(eval(call, R_BaseEnv));
  
  if (LENGTH(newbuf) == 0) {
    /* we must be non-blocking and have either run out, or cone to the end of the buffer?*/
    error("implementation needed");
  }
  error("implementation needed");
  assert_type(newbuf, RAWSXP);
  return(0);
  return LENGTH(newbuf);
}
