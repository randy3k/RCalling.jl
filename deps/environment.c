#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

extern SEXP sexp_eval_promise(SEXP s);

SEXP RCall_GlobalEnv()
{
    return R_GlobalEnv;
}

SEXP RCall_BaseEnv()
{
    return R_BaseEnv;
}

SEXP RCall_findVar(char *v, SEXP env)
{
    SEXP fun;
    fun = PROTECT(findVar(Rf_install(v), env));
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
    R_PreserveObject(fun);
    return fun;
}
