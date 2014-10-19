#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

extern jl_value_t *rj_wrap(SEXP ss);

int sexp_named(const SEXP s)
{
    int res = NAMED(s);
    return res;
}

int sexp_typeof(const SEXP s)
{
    int res = TYPEOF(s);
    return res;
}

int sexp_length(const SEXP s)
{
    int res = LENGTH(s);
    return res;
}

int sexp_ndims(const SEXP s)
{
    SEXP dims = Rf_getAttrib(s, R_DimSymbol);
    int res;
    if (Rf_isNull(dims))
        res = 1;
    else
        res = LENGTH(dims);
    return res;
}

jl_tuple_t *sexp_size(const SEXP s)
{
    jl_tuple_t *d;
    SEXP dims = Rf_getAttrib(s, R_DimSymbol);

    if (dims != R_NilValue)
    {
        //array or matrix
        int ndims = LENGTH(dims);
        d = jl_alloc_tuple(ndims);
        JL_GC_PUSH1(&d);
        size_t i;
        for (i = 0; i < ndims; i++)
        {
            jl_tupleset(d, i, jl_box_long(INTEGER(dims)[i]));
        }
        JL_GC_POP();
    }
    else
    {
        //list
        d = jl_alloc_tuple(1);
        JL_GC_PUSH1(&d);
        jl_tupleset(d, 0, jl_box_long(LENGTH(s)));
        JL_GC_POP();
    }
    return d;
}

jl_array_t *sexp_names(const SEXP s)
{
    SEXP res = Rf_getAttrib(s, R_NamesSymbol);
    return (jl_array_t *) rj_wrap(res);
}

SEXP sexp_get_attr(const SEXP s, char *name)
{
    SEXP res = Rf_getAttrib(s, Rf_install(name));
    if (Rf_isNull(res))
    {
        res = NULL;
    } else {
        R_PreserveObject(res);
    }
    return res;
}

// one argument subset
SEXP sexp_subset(const SEXP s, const SEXP i)
{
    int errorOccurred;
    SEXP fun, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset"),  R_GlobalEnv));
    ret = R_tryEval(Rf_lang3(fun, s, i), R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(1);
    return ret;
}

// two arguments subset
SEXP sexp_subset2(const SEXP s, const SEXP i, const SEXP j)
{
    int errorOccurred;
    SEXP fun, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset"),  R_GlobalEnv));
    ret = R_tryEval(Rf_lang4(fun, s, i, j), R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(1);
    return ret;
}

// list subset
SEXP sexp_listsubset(const SEXP s, const SEXP i)
{
    int errorOccurred;
    SEXP fun, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset2"),  R_GlobalEnv));
    ret = R_tryEval(Rf_lang3(fun, s, i), R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(1);
    return ret;
}

void *sexp_pointer(const SEXP x)
{
    void *p;
    switch (TYPEOF(x))
    {
        case LGLSXP:
            p = LOGICAL(x);
            break;
        case INTSXP:
            p = INTEGER(x);
            break;
        case REALSXP:
            p = REAL(x);
            break;
        default:
            p = 0;
    }
    return p;
}

void sexp_print(const SEXP s)
{
    int errorOccurred;
    SEXP fun;
    fun = PROTECT(Rf_findFun(Rf_install("print"),  R_GlobalEnv));
    R_tryEval(Rf_lang2(fun, s), R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
}
