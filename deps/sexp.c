#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

extern jl_value_t *rj_cast(SEXP ss);
extern SEXP jr_cast(const jl_value_t *tt);


#define UTF8_MASK (1<<3)
#define ASCII_MASK (1<<6)
#define IS_ASCII(x) (ENVFLAGS(x) & ASCII_MASK)
#define IS_UTF8(x) (ENVFLAGS(x) & UTF8_MASK)

int sexp_mark(SEXP ss)
{
    return MARK(ss);
}

int sexp_is_ascii(SEXP ss)
{
    if (TYPEOF(ss) != STRSXP)
        jl_error("Expect string object.");
    size_t n = LENGTH(ss);
    SEXP t;
    for (size_t i=0; i<n; i++)
    {
        t = STRING_ELT(ss, i);
        if (t == NA_STRING)
            continue;
        if (! IS_ASCII(STRING_ELT(ss, i)))
            return 0;
    }
    return 1;
}

int sexp_named(const SEXP ss)
{
    int res = NAMED(ss);
    return res;
}

int sexp_typeof(const SEXP ss)
{
    int res = TYPEOF(ss);
    return res;
}

int sexp_length(const SEXP ss)
{
    int res = LENGTH(ss);
    return res;
}

int sexp_ndims(const SEXP ss)
{
    SEXP dims = Rf_getAttrib(ss, R_DimSymbol);
    int res;
    if (Rf_isNull(dims))
        res = 1;
    else
        res = LENGTH(dims);
    return res;
}

jl_tuple_t *sexp_size(const SEXP ss)
{
    jl_tuple_t *d;
    SEXP dims = Rf_getAttrib(ss, R_DimSymbol);

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
        JL_GC_PUSH1(&d);
        d = jl_tuple1(jl_box_long(LENGTH(ss)));
        JL_GC_POP();
    }
    return d;
}

jl_array_t *sexp_names(const SEXP ss)
{
    SEXP res = Rf_getAttrib(ss, R_NamesSymbol);
    if (res == R_NilValue)
        return JL_NULL;
    return (jl_array_t *) rj_cast(res);
}

SEXP sexp_get_attr(const SEXP ss, char *name)
{
    SEXP res = Rf_getAttrib(ss, Rf_install(name));
    if (Rf_isNull(res))
        res = NULL;
    return res;
}

// one argument subset
SEXP sexp_getindex_(const SEXP ss, const SEXP i)
{
    int errorOccurred;
    SEXP e, ret;
    e = PROTECT(Rf_lang3(Rf_install(".subset"), ss, i));
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
    return ret;
}

SEXP sexp_getindex(const SEXP ss, const jl_value_t* i)
{
    SEXP ir = PROTECT(jr_cast(i));
    SEXP ret = R_NilValue;
    ret = sexp_getindex_(ss, ir);
    UNPROTECT(1);
    return ret;
}

// two arguments subset
SEXP sexp_getindex2_(const SEXP ss, const SEXP i, const SEXP j)
{
    int errorOccurred;
    SEXP e, ret;
    e = PROTECT(Rf_lang4(Rf_install(".subset"), ss, i, j));
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
    return ret;
}

SEXP sexp_getindex2(const SEXP ss, const jl_value_t* i, const jl_value_t* j)
{
    SEXP ir = PROTECT(jr_cast(i));
    SEXP jr = PROTECT(jr_cast(j));
    SEXP ret = R_NilValue;
    ret = sexp_getindex2_(ss, ir, jr);
    UNPROTECT(2);
    return ret;
}

// TODO: multiple arguments getter

// list subset
SEXP sexp_list_getindex(const SEXP ss, const SEXP i)
{
    int errorOccurred;
    SEXP e, ret;
    e = PROTECT(Rf_lang3(Rf_install(".subset2"), ss, i));
    ret = R_tryEval(e, R_GlobalEnv, &errorOccurred);
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

void sexp_print(const SEXP ss)
{
    Rf_PrintValueRec(ss, R_GlobalEnv);
}
