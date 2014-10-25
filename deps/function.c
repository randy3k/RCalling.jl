#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

extern int jl_isa(jl_value_t* tt, char* type);
extern SEXP jr_cast(jl_value_t *tt);


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

SEXP make_call(SEXP fun, jl_array_t *argv, jl_array_t *argn)
{
    // make call
    int argc = jl_array_len(argv);
    int count = 0;
    SEXP s, t;
    PROTECT(s = t = Rf_allocVector(LANGSXP, argc+1));
    count++;
    SETCAR(t, fun);
    t = CDR(t);
    /* iterate over the arguments */
    int i;
    jl_value_t *argvi;
    SEXP u;
    char *name;
    for (i = 0; i < argc; i++)
    {
        argvi = jl_arrayref(argv, i);
        if (jl_isa(argvi, "RAny"))
        {
            u = jl_unbox_voidpointer(jl_get_nth_field(argvi, 0));
        }
        else
        {
            u = PROTECT(jr_cast(argvi));
            count++;
        }
        SETCAR(t, u);
        name = jl_string_data(jl_arrayref(argn, i));
        if (strlen(name) > 0) {
            SET_TAG(t, Rf_install(name));
        }
        t = CDR(t);
    }
    UNPROTECT(count);
    return s;
}

SEXP rcall(SEXP fun, jl_array_t *argv, jl_array_t *argn, SEXP env)
{
    int count = 0;
    SEXP ret;
    int errorOccurred = 0;

    SEXP e = PROTECT(make_call(fun, argv, argn));
    count++;
    ret = PROTECT(R_tryEval(e, env, &errorOccurred));
    count++;
    if (errorOccurred)
    {
        UNPROTECT(count);
        return R_NilValue;
    }
    if (TYPEOF(ret) == PROMSXP)
        ret = sexp_eval_promise(ret);

    UNPROTECT(count);
    return ret;
}
