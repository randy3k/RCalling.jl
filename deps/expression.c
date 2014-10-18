#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>
#include <julia.h>


SEXP RCall_parse(SEXP cmd)
{
    SEXP expr;
    ParseStatus status;
    expr = PROTECT(R_ParseVector(cmd, -1, &status, R_NilValue));
    if (status != PARSE_OK)
    {
        UNPROTECT(1);
        jl_error("R parser error.");
        return R_NilValue;
    }
    UNPROTECT(1);
    R_PreserveObject(expr);
    return expr;
}

SEXP RCall_eval(SEXP e, SEXP env)
{
    int errorOccurred;
    int count = 0;
    SEXP ans = R_NilValue;
    for (int i = 0; i < LENGTH(e); i++)
    {
        ans = PROTECT(R_tryEval(VECTOR_ELT(e, i), env, &errorOccurred));
        count++;
        if (errorOccurred)
        {
            jl_error("R eval error.");
            return R_NilValue;
        }
    }
    UNPROTECT(count);
    R_PreserveObject(ans);
    return ans;
}
