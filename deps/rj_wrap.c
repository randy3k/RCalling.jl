#include <stdio.h>
#define USE_RINTERNALS
#include <Rdefines.h>
#include <stdbool.h>
#include <julia.h>
// some of the followings are adapted from https://github.com/armgong/RJulia/blob/master/src/R_Julia.c

#define UTF8_MASK (1<<3)
#define ASCII_MASK (1<<6)
#define IS_ASCII(x) ((x)->sxpinfo.gp & ASCII_MASK)
#define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)

static jl_tuple_t *rj_dim(SEXP ss)
{
    jl_tuple_t *d;
    SEXP dims = getAttrib(ss, R_DimSymbol);
    //array or matrix
    if (dims != R_NilValue)
    {
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
    //vector
    {
        d = jl_alloc_tuple(1);
        JL_GC_PUSH1(&d);
        jl_tupleset(d, 0, jl_box_long(LENGTH(ss)));
        JL_GC_POP();
    }
    return d;
}

static inline jl_array_t *rj_array(jl_datatype_t *type, void* data, jl_tuple_t *dims)
{
  return jl_ptr_to_array(jl_apply_array_type(type, jl_tuple_len(dims)), data, dims, 0);
}

static jl_array_t *rj_new_array(jl_datatype_t *type, jl_tuple_t *dims)
{
  return jl_new_array(jl_apply_array_type(type, jl_tuple_len(dims)), dims);
}

jl_value_t *rj_wrap(SEXP ss)
{
    jl_value_t *ret = jl_nothing;
    if ((LENGTH(ss)) != 0)
    {
        jl_tuple_t *dims = rj_dim(ss);
        switch (TYPEOF(ss))
        {
            case LGLSXP:
            {
                ret = (jl_value_t *) rj_new_array(jl_bool_type, dims);
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
                    ret = (jl_value_t *) rj_new_array(jl_utf8_string_type, dims);
                else
                    ret = (jl_value_t *) rj_new_array(jl_ascii_string_type, dims);
                JL_GC_PUSH1(&ret);
                jl_value_t **data = jl_array_data(ret);
                for (size_t i = 0; i < jl_array_len(ret); i++)
                    if (!IS_ASCII(ss))
                        data[i] = jl_cstr_to_string(translateChar0(STRING_ELT(ss, i)));
                    else
                        data[i] = jl_cstr_to_string(CHAR(STRING_ELT(ss, i)));
                JL_GC_POP();
                break;
            }
            case VECSXP:
            {
                // TODO: convert to Dict
                ret = (jl_value_t *) jl_alloc_tuple(length(ss));
                JL_GC_PUSH1(&ret);
                for (int i = 0; i < length(ss); i++)
                {
                    jl_tupleset(ret, i, rj_wrap(VECTOR_ELT(ss, i)));
                }
                JL_GC_POP();
                break;
            }
            default:
            {
                ret = jl_nothing;
            }
        }
    }
    return (jl_value_t *) ret;
}
