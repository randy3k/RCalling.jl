#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>

SEXP sexp_eval_promise(const SEXP s)
{
    SEXP env, t;
    PROTECT(env = PRENV(s));
    if (env == R_NilValue)
    {
        UNPROTECT(1);
        PROTECT(env = R_GlobalEnv);
    }
    PROTECT(t = eval(s, env));
    // R_PreserveObject(t);
    UNPROTECT(2);
    return t;
}

SEXP RCall_call(SEXP fun_R, SEXP *argv, int argc, char **argn, SEXP env)
{

    // make call
    int count = 0;
    SEXP s, t;
    PROTECT(s = t = allocVector(LANGSXP, argc+1));
    count++;
    SETCAR(t, fun_R);
    t = CDR(t);
    /* iterate over the arguments */
    int arg_i;
    char *arg_name;
    for (arg_i = 0; arg_i < argc; arg_i++)
    {
        SETCAR(t, argv[arg_i]);
        arg_name = argn[arg_i];
        if (strlen(arg_name) > 0) {
            SET_TAG(t, install(arg_name));
        }
        t = CDR(t);
    }

    // call
    int errorOccurred = 0;
    SEXP res_R;
    res_R = PROTECT(R_tryEval(s, env, &errorOccurred));
    count++;
    if (errorOccurred)
    {
        UNPROTECT(count);
        return NULL;
    }
    SEXP res_ok;
    if (TYPEOF(res_R) == PROMSXP)
    {
        res_ok = sexp_eval_promise(res_R);
    }
    else
    {
        res_ok = res_R;
    }
    if (errorOccurred) {
        res_R = R_NilValue;
    }
    else
    {
        R_PreserveObject(res_ok);
    }
    UNPROTECT(count);
    return res_ok;
}
