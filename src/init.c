#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern SEXP C_qc_precip_row_stats(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef call_methods[] = {
    {".qc_precip_row_stats", (DL_FUNC) &C_qc_precip_row_stats, 4},
    {NULL, NULL, 0}
};

void attribute_visible R_init_hydroTSM(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
