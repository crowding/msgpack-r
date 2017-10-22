#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _pack_msg(SEXP, SEXP);
extern SEXP _pack_opts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _unpack_msg(SEXP);
extern SEXP _unpack_msg_partial(SEXP, SEXP, SEXP);
extern SEXP _unpack_opts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_pack_msg",           (DL_FUNC) &_pack_msg,           2},
    {"_pack_opts",          (DL_FUNC) &_pack_opts,          6},
    {"_unpack_msg",         (DL_FUNC) &_unpack_msg,         1},
    {"_unpack_msg_partial", (DL_FUNC) &_unpack_msg_partial, 3},
    {"_unpack_opts",        (DL_FUNC) &_unpack_opts,        8},
    {NULL, NULL, 0}
};

void R_init_msgpack(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
