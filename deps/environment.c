#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

extern SEXP sexp_eval_promise(SEXP s);

SEXP rcall_global_env()
{
    return R_GlobalEnv;
}

SEXP rcall_base_env()
{
    return R_BaseEnv;
}

SEXP rcall_findVar(char *v, SEXP env)
{
    SEXP fun;
    fun = PROTECT(Rf_findVar(Rf_install(v), env));
    if (fun == R_UnboundValue)
    {
        UNPROTECT(1);
        return R_NilValue;
    }
    if (TYPEOF(fun) == PROMSXP)
    {
        fun = sexp_eval_promise(fun);
    }
    UNPROTECT(1);
    return fun;
}
