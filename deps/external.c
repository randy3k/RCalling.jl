#include <stdio.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Parse.h>
#include <julia.h>

extern SEXP jr_wrap(jl_value_t *tt, int preserve);
extern jl_value_t *rj_wrap(SEXP ss);

SEXP do_julia(SEXP args)
{
    SEXP v;

    args = CDR(args);
    jl_function_t *func = R_ExternalPtrAddr(CAR(args));
    jl_value_t **jargs;
    jl_value_t *ans;

    v = args;
    int count=0;
    for (v = CDR(v); v != R_NilValue; v = CDR(v))
        count++;

    int i ;
    JL_GC_PUSHARGS(jargs, count);
    v = args;
    for (v = CDR(v), i = 0; v != R_NilValue; v = CDR(v), i++)
    {
        jargs[i] = rj_wrap(CAR(v));
    }
    ans = jl_apply(func, jargs, count);
    JL_GC_POP();

    if (ans == NULL)
        return R_NilValue;
    return jr_wrap(ans, 1);
}

void RCall_registerRoutines()
{
    R_ExternalMethodDef externalMethods[] = {
        {".RCall", (DL_FUNC) &do_julia, -1},
        {NULL, NULL, 0}
    };
    R_registerRoutines(R_getEmbeddingDllInfo(), NULL, NULL, NULL, externalMethods);
}


SEXP jr_func_wrap(void* p)
{
    ParseStatus status;
    SEXP cmdSexp, s, t, ext;
    cmdSexp =  PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(cmdSexp, 0, mkChar("function(...) {.External(\".RCall\", NULL, ...)}"));
    s = t = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    ext = PROTECT(R_MakeExternalPtr(p, R_NilValue, R_NilValue));
    SETCADDR(CADR(CADDR(VECTOR_ELT(t ,0))), ext);

    int errorOccurred = 0;
    SEXP ret;
    ret = PROTECT(R_tryEval(VECTOR_ELT(s,0), R_GlobalEnv, &errorOccurred));
    R_PreserveObject(ret);
    UNPROTECT(4);
    return ret;
}
