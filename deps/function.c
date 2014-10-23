#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
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
    PROTECT(t = Rf_eval(s, env));
    UNPROTECT(2);
    return t;
}

SEXP rcall_call(SEXP fun_R, SEXP *argv, int argc, char **argn, SEXP env)
{

    // make call
    int count = 0;
    SEXP s, t;
    PROTECT(s = t = Rf_allocVector(LANGSXP, argc+1));
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
            SET_TAG(t, Rf_install(arg_name));
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
    UNPROTECT(count);
    return res_ok;
}

SEXP jr_func_wrap(void* p)
{
    ParseStatus status;
    SEXP s, t, ext;
    s = t = PROTECT(R_ParseVector(
        Rf_mkString("function(...) {.External(\".RCall\", NULL, ...)}"),
        -1, &status, R_NilValue));
    ext = PROTECT(R_MakeExternalPtr(p, R_NilValue, R_NilValue));
    SETCADDR(CADR(CADDR(VECTOR_ELT(t ,0))), ext);
    int errorOccurred = 0;
    SEXP ret;
    ret = PROTECT(R_tryEval(VECTOR_ELT(s,0), R_GlobalEnv, &errorOccurred));
    UNPROTECT(3);
    return ret;
}
