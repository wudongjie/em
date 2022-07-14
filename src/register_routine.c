#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP _em_mix_ll(SEXP thetaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP dSEXP, SEXP latentSEXP, SEXP familySEXP, SEXP isLogSEXP, SEXP constraintSEXP);
SEXP _em_pi_ll(SEXP alphaSEXP, SEXP XSEXP, SEXP dSEXP, SEXP latentSEXP);
SEXP _em_post_pr(SEXP thetaSEXP, SEXP pi_mSEXP, SEXP YSEXP, SEXP XSEXP, SEXP latentSEXP, SEXP familySEXP, SEXP constraintSEXP);

SEXP coxcount1(SEXP y2, SEXP strat2) {
  static SEXP(*fun)(SEXP,SEXP) = NULL;
  if (fun==NULL) {
    fun = (SEXP(*)(SEXP, SEXP)) R_GetCCallable("survival", "Ccoxcount1");
    if (fun==NULL) Rf_error("cannot find function 'survival_Ccoxcount1'");
  }
  return(fun(y2, strat2));
};

SEXP coxcount2(SEXP y2, SEXP isort1, SEXP isort2, SEXP strat2){
  static SEXP(*fun)(SEXP,SEXP,SEXP,SEXP) = NULL;
  if (fun==NULL) {
    fun = (SEXP(*)(SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("survival", "Ccoxcount2");
    if (fun==NULL) Rf_error("cannot find function 'survival_Ccoxcount2'");
  }
  return(fun(y2, isort1, isort2, strat2));
};

static const R_CallMethodDef CallEntries[] = {
  {"_em_mix_ll", (DL_FUNC) &_em_mix_ll, 8},
  {"_em_pi_ll", (DL_FUNC) &_em_pi_ll, 4},
  {"_em_post_pr", (DL_FUNC) &_em_post_pr, 7},
  {NULL, NULL, 0}
};

void R_init_em(DllInfo *dll) {
  R_RegisterCCallable("em", "Ccoxcount1", (DL_FUNC) coxcount1);
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}