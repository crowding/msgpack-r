#ifndef CONN_H

#define CONN_H

typedef struct read_cxt {
  unpack_opts opts;
  cw_unpack_context opts;
  SEXP buf;
  SEXP backlog;
} read_cxt;

typedef struct write_cxt {
  unpack_opts opts;
  cw_unpack_context opts;
  SEXP buf;
  SEXP backlog;
} write_cxt;

#endif 



