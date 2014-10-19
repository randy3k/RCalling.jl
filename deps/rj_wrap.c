#define R_NO_REMAP
#include <stdbool.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <julia.h>
// some of the followings are adapted from https://github.com/armgong/RJulia/blob/master/src/R_Julia.c

#define UTF8_MASK (1<<3)
#define ASCII_MASK (1<<6)
#define IS_ASCII(x) (ENVFLAGS(x) & ASCII_MASK)
#define IS_UTF8(x) (ENVFLAGS(x) & UTF8_MASK)

extern jl_tuple_t *sexp_size(const SEXP s);
extern jl_array_t *sexp_names(const SEXP s);
jl_value_t *rj_wrap(SEXP ss);

static inline jl_array_t *rj_array(jl_datatype_t *type, void* data, jl_tuple_t *dims)
{
  return jl_ptr_to_array(jl_apply_array_type(type, jl_tuple_len(dims)), data, dims, 0);
}

static inline jl_array_t *new_array(jl_datatype_t *type, jl_tuple_t *dims)
{
  return jl_new_array(jl_apply_array_type(type, jl_tuple_len(dims)), dims);
}

static inline bool IS_NAMED(SEXP ss)
{
    SEXP name = Rf_getAttrib(ss, R_NamesSymbol);
    SEXP fun, v;
    if (name == R_NilValue) return 0;
    int errorOccurred;
    fun = PROTECT(Rf_findFun(Rf_install("nchar"),  R_GlobalEnv));
    v = R_tryEval(Rf_lang2(fun, name), R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
    if (v == R_NilValue) return 0;
    for(int i=0; i<LENGTH(v); i++)
    {
        if (INTEGER(v)[i]==0)
        {
            return 0;
        }
    }
    return 1;
}

static inline bool ISA(SEXP ss, const char *name)
{
    SEXP cls = Rf_getAttrib(ss, R_ClassSymbol);
    if (cls == R_NilValue) return 0;
    if (strcmp(CHAR(STRING_ELT(cls, 0)), name)==0) return 1;
    return 0;
}

jl_value_t *rj_data_frame(SEXP ss)
{
    jl_value_t *ret = jl_nothing;
    if (TYPEOF(ss) == VECSXP)
    {
        SEXP rownames = PROTECT(Rf_getAttrib(ss, R_RowNamesSymbol));
        int align = 0;
        if (TYPEOF(rownames) == STRSXP)
            align = 1;
        size_t n = LENGTH(ss);
        jl_array_t *columns = new_array(jl_any_type, jl_tuple1(jl_box_int64(n+align)));
        jl_array_t *names = sexp_names(ss);
        jl_array_t *sym = new_array(jl_symbol_type, jl_tuple1(jl_box_int64(n+align)));
        JL_GC_PUSH4(&columns, &names, &sym, &ret);

        if (align)
        // if row names are not string
        {
            jl_arrayset(columns, rj_wrap(rownames), 0);
            jl_arrayset(sym, (jl_value_t *) jl_symbol("RowID"), 0);
        }
        for (int i = 0; i < n; i++)
        {
            jl_arrayset(columns, rj_wrap(VECTOR_ELT(ss, i)), i+align);
            jl_arrayset(sym, (jl_value_t *) jl_symbol(jl_string_data(jl_arrayref(names, i))), i+align);
        }
        jl_function_t *func = jl_get_function(jl_current_module, "DataFrame");
        ret = jl_call2(func, (jl_value_t *) columns, (jl_value_t *) sym);
        UNPROTECT(1);
        JL_GC_POP();
    }
    return ret;
}

jl_value_t *rj_list(SEXP ss)
{
    jl_value_t *ret;
    size_t n = LENGTH(ss);
    jl_datatype_t *ttype = (jl_datatype_t *) jl_eval_string("(Any, Any)");
    ret = (jl_value_t *) new_array(ttype, jl_tuple1(jl_box_int64(n)));
    JL_GC_PUSH1(&ret);
    if (IS_NAMED(ss))
    {
        jl_array_t *names = sexp_names(ss);
        for (int i = 0; i < n; i++)
        {
            jl_arrayset((jl_array_t *) ret,
                (jl_value_t *) jl_tuple2(jl_arrayref(names, i), rj_wrap(VECTOR_ELT(ss, i))), i);
        }
    }
    else
    {
        for (int i = 0; i < n; i++)
        {
            jl_arrayset((jl_array_t *) ret,
                (jl_value_t *) jl_tuple2(jl_box_int64(i+1), rj_wrap(VECTOR_ELT(ss, i))), i);
        }
    }
    jl_function_t *func = jl_get_function(jl_base_module, "Dict");
    ret = jl_call1(func, ret);
    JL_GC_POP();
    return ret;
}

jl_value_t *rj_wrap(SEXP ss)
{
    jl_value_t *ret = jl_nothing;
    if ((LENGTH(ss)) != 0)
    {
        jl_tuple_t *dims = sexp_size(ss);
        switch (TYPEOF(ss))
        {
            case LGLSXP:
            {
                ret = (jl_value_t *) new_array(jl_bool_type, dims);
                JL_GC_PUSH1(&ret);
                bool *data = (bool *)jl_array_data(ret);
                for (size_t i = 0; i < jl_array_len(ret); i++)
                    data[i] = LOGICAL(ss)[i];
                JL_GC_POP();
                break;
            };
            case INTSXP:
            {
                int *data = INTEGER(ss);
                ret = (jl_value_t *) rj_array(jl_int32_type, data, dims);
                break;
            }
            case REALSXP:
            {
                double *data = REAL(ss);
                ret = (jl_value_t *) rj_array(jl_float64_type, data, dims);
                break;
            }
            case STRSXP:
            {
                if (!IS_ASCII(ss))
                    ret = (jl_value_t *) new_array(jl_utf8_string_type, dims);
                else
                    ret = (jl_value_t *) new_array(jl_ascii_string_type, dims);
                JL_GC_PUSH1(&ret);
                jl_value_t **data = jl_array_data(ret);
                for (size_t i = 0; i < jl_array_len(ret); i++)
                    if (!IS_ASCII(ss))
                        data[i] = jl_cstr_to_string(Rf_translateChar0(STRING_ELT(ss, i)));
                    else
                        data[i] = jl_cstr_to_string(CHAR(STRING_ELT(ss, i)));
                JL_GC_POP();
                break;
            }
            case VECSXP:
            {
                if (ISA(ss, "data.frame"))
                    ret = rj_data_frame(ss);
                else
                    ret = rj_list(ss);
                break;
            }
            default:
            {
                jl_error("RCall does not know to convert this R object.");
                ret = jl_nothing;
            }
        }
    }
    return (jl_value_t *) ret;
}
