#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Parse.h>
#include <julia.h>

extern SEXP jr_cast(jl_value_t *tt, int preserve);
extern jl_value_t *rj_cast(SEXP ss);

static SEXP do_julia(SEXP args)
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
        jargs[i] = rj_cast(CAR(v));
    }
    ans = jl_apply(func, jargs, count);
    JL_GC_POP();

    if (ans == NULL)
        return R_NilValue;
    return jr_cast(ans, 1);
}

void rcall_register_routines()
{
    R_ExternalMethodDef externalMethods[] = {
        {".RCall", (DL_FUNC) &do_julia, -1},
        {NULL, NULL, 0}
    };
    R_registerRoutines(R_getEmbeddingDllInfo(), NULL, NULL, NULL, externalMethods);
}
