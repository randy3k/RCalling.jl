#include <stdlib.h>
#include <stdio.h>
#include <Rdefines.h>

int sexp_named(const SEXP sexp)
{
    int res = NAMED(sexp);
    return res;
}

int sexp_typeof(const SEXP sexp)
{
    int res = TYPEOF(sexp);
    return res;
}

int sexp_length(const SEXP sexp)
{
    int res = LENGTH(sexp);
    return res;
}

int sexp_ndims(const SEXP sexp)
{
    SEXP dims = GET_DIM(sexp);
    int res;
    if (Rf_isNull(dims))
        res = 1;
    else
        res = LENGTH(dims);
    return res;
}

SEXP sexp_size(const SEXP sexp)
{
    SEXP dims = GET_DIM(sexp);
    if (Rf_isNull(dims))
    {
        return NULL;
    }
    R_PreserveObject(dims);
    return dims;
}

SEXP sexp_names(const SEXP sexp)
{
    SEXP res = GET_NAMES(sexp);
    R_PreserveObject(res);
    return res;
}

SEXP sexp_get_attr(const SEXP sexp, char *name)
{
    SEXP res = getAttrib(sexp, Rf_install(name));
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
    SEXP fun, e;
    fun = PROTECT(Rf_findFun(Rf_install("print"),  R_GlobalEnv));
    e = PROTECT(allocVector(LANGSXP, 2));
    SETCAR(e, fun);
    SETCAR(CDR(e), s);
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(2);
}

// one argument subset
SEXP sexp_subset(const SEXP s, const SEXP i)
{
    int errorOccurred;
    SEXP fun, e, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset"),  R_GlobalEnv));
    e = PROTECT(allocVector(LANGSXP, 3));
    SETCAR(e, fun);
    SETCAR(CDR(e), s);
    SETCAR(CDDR(e), i);
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(2);
    return ret;
}

// two arguments subset
SEXP sexp_subset2(const SEXP s, const SEXP i, const SEXP j)
{
    int errorOccurred;
    SEXP fun, e, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset"),  R_GlobalEnv));
    e = PROTECT(allocVector(LANGSXP, 4));
    SETCAR(e, fun);
    SETCADR(e, s);
    SETCADDR(e, i);
    SETCADDDR(e, j);
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(2);
    return ret;
}

// list subset
SEXP sexp_listsubset(const SEXP s, const SEXP i)
{
    int errorOccurred;
    SEXP fun, e, ret;
    fun = PROTECT(Rf_findFun(Rf_install(".subset2"),  R_GlobalEnv));
    e = PROTECT(allocVector(LANGSXP, 3));
    SETCAR(e, fun);
    SETCAR(CDR(e), s);
    SETCAR(CDDR(e), i);
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
    R_PreserveObject(ret);
    UNPROTECT(2);
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

void * sexp_str_pointer(const SEXP x)
{
    char **p;
    int i, n;
    switch (TYPEOF(x))
    {
        case STRSXP:
            n = LENGTH(x);
            p = (char**) malloc(sizeof(char*) * n);
            // p is freed in julia
            for(i=0;i<n;i++)
                p[i] = (char*) CHAR(STRING_ELT(x,i));
            break;
        default:
            p = 0;
    }
    return p;
}
