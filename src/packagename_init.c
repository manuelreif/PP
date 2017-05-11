#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP PP_ansol(SEXP, SEXP);
extern SEXP PP_L12gpcm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_L12gpcm_robust(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_L12gpcm_wle(SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_L4pl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_L4pl_robust(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_L4pl_wle(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_Lgpcm4pl_mle(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_Lgpcm4pl_robust(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_Lgpcm4pl_wle(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_Likgpcm(SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_NR_4PL(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_NR_GPCM(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_NR_mixed(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_P_4pl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_P_4pl4wle(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_Pcorr1_gpcm(SEXP, SEXP, SEXP);
extern SEXP PP_P_gpcm(SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_r_huber_4pl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_r_huber_gpcm(SEXP, SEXP, SEXP, SEXP);
extern SEXP PP_sim_4pl(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"PP_ansol",           (DL_FUNC) &PP_ansol,            2},
    {"PP_L12gpcm",         (DL_FUNC) &PP_L12gpcm,          7},
    {"PP_L12gpcm_robust",  (DL_FUNC) &PP_L12gpcm_robust,   5},
    {"PP_L12gpcm_wle",     (DL_FUNC) &PP_L12gpcm_wle,      4},
    {"PP_L4pl",            (DL_FUNC) &PP_L4pl,             9},
    {"PP_L4pl_robust",     (DL_FUNC) &PP_L4pl_robust,      7},
    {"PP_L4pl_wle",        (DL_FUNC) &PP_L4pl_wle,         6},
    {"PP_Lgpcm4pl_mle",    (DL_FUNC) &PP_Lgpcm4pl_mle,    10},
    {"PP_Lgpcm4pl_robust", (DL_FUNC) &PP_Lgpcm4pl_robust,  8},
    {"PP_Lgpcm4pl_wle",    (DL_FUNC) &PP_Lgpcm4pl_wle,     7},
    {"PP_Likgpcm",         (DL_FUNC) &PP_Likgpcm,          4},
    {"PP_NR_4PL",          (DL_FUNC) &PP_NR_4PL,          12},
    {"PP_NR_GPCM",         (DL_FUNC) &PP_NR_GPCM,         10},
    {"PP_NR_mixed",        (DL_FUNC) &PP_NR_mixed,        13},
    {"PP_P_4pl",           (DL_FUNC) &PP_P_4pl,            5},
    {"PP_P_4pl4wle",       (DL_FUNC) &PP_P_4pl4wle,        5},
    {"PP_Pcorr1_gpcm",     (DL_FUNC) &PP_Pcorr1_gpcm,      3},
    {"PP_P_gpcm",          (DL_FUNC) &PP_P_gpcm,           4},
    {"PP_r_huber_4pl",     (DL_FUNC) &PP_r_huber_4pl,      6},
    {"PP_r_huber_gpcm",    (DL_FUNC) &PP_r_huber_gpcm,     4},
    {"PP_sim_4pl",         (DL_FUNC) &PP_sim_4pl,          5},
    {NULL, NULL, 0}
};

void R_init_PP(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
