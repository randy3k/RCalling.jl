#include <R.h>
#include <Rinternals.h>

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
    SEXP dims = getAttrib(s, R_DimSymbol);
    int res;
    if (Rf_isNull(dims))
        res = 1;
    else
        res = LENGTH(dims);
    return res;
}

SEXP sexp_size(const SEXP s)
{
    SEXP dims = getAttrib(s, R_DimSymbol);
    if (Rf_isNull(dims))
    {
        return NULL;
    }
    R_PreserveObject(dims);
    return dims;
}

SEXP sexp_names(const SEXP s)
{
    SEXP res = getAttrib(s, R_NamesSymbol);
    R_PreserveObject(res);
    return res;
}

SEXP sexp_get_attr(const SEXP s, char *name)
{
    SEXP res = getAttrib(s, Rf_install(name));
    if (Rf_isNull(res))
    {
        res = NULL;
    } else {
        R_PreserveObject(res);
    }
    return res;
}

void sexp_print(const SEXP s)
{
    int errorOccurred;
    SEXP fun;
    fun = PROTECT(Rf_findFun(Rf_install("print"),  R_GlobalEnv));
    R_tryEval(Rf_lang2(fun, s), R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
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

void * sexp_pointer(const SEXP x)
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
