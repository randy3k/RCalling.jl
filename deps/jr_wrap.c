#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>
#include <julia.h>
// mostly adapted from https://github.com/armgong/RJulia/blob/master/src/Julia_R.c

SEXP jr_wrap(jl_value_t *tt, bool own);

#define in_int32_range(x) x<=INT32_MAX && x>=INT32_MIN

static inline bool jl_isa(jl_value_t* tt, char* type)
{
    jl_value_t* atype = jl_get_global(jl_current_module, jl_symbol(type));
    if ((atype == NULL) || (! jl_is_datatype(atype)))
        return 0;
    return jl_subtype(tt, atype, 1);
}

SEXP jr_scalar(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    double tmpfloat;
    // float64, int64, int32 are most common, so put them in the front
    if (jl_is_float64(tt))
    {
        PROTECT(ans = ScalarReal(jl_unbox_float64(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int32(tt))
    {
        PROTECT(ans = ScalarInteger(jl_unbox_int32(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int64(tt))
    {
        tmpfloat=(double)jl_unbox_int64(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = ScalarInteger((int32_t)jl_unbox_int64(tt)));
        else
            PROTECT(ans = ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_bool(tt))
    {
        PROTECT(ans = ScalarLogical(jl_unbox_bool(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int8(tt))
    {
        PROTECT(ans = ScalarInteger(jl_unbox_int8(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint8(tt))
    {
        PROTECT(ans = ScalarInteger(jl_unbox_uint8(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int16(tt))
    {
        PROTECT(ans = ScalarInteger(jl_unbox_int16(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint16(tt))
    {
        PROTECT(ans = ScalarInteger(jl_unbox_uint16(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint32(tt))
    {
        tmpfloat=(double)jl_unbox_uint32(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = ScalarInteger((int32_t)jl_unbox_uint32(tt)));
        else
            PROTECT(ans = ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_uint64(tt))
    {
        tmpfloat=(double)jl_unbox_int64(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = ScalarInteger((int32_t)jl_unbox_uint64(tt)));
        else
            PROTECT(ans = ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_float32(tt))
    {
        PROTECT(ans = ScalarReal(jl_unbox_float32(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_utf8_string(tt))
    {
        PROTECT(ans = allocVector(STRSXP, 1));
        SET_STRING_ELT(ans, 0, mkCharCE(jl_string_data(tt), CE_UTF8));
        UNPROTECT(1);
    }
    else if (jl_is_ascii_string(tt))
    {
        PROTECT(ans = ScalarString(mkChar(jl_string_data(tt))));
        UNPROTECT(1);
    }
    return ans;
}

SEXP jr_array(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    jl_value_t *val;
    if (((jl_array_t *)tt)->ptrarray)
        val = jl_cellref(tt, 0);
    else
        val = jl_arrayref((jl_array_t *)tt, 0);
    //get Julia dims and set R array Dims
    size_t len = jl_array_len(tt);
    if (len == 0)
        return ans;

    int ndims = jl_array_ndims(tt);
    SEXP dims;
    PROTECT(dims = allocVector(INTSXP, ndims));
    for (size_t i = 0; i < ndims; i++)
    {
        INTEGER(dims)[i] = jl_array_dim(tt, i);
    }
    UNPROTECT(1);

    // again, float64, int32 and int64 are most common
    if (jl_is_float64(val))
    {
            double *p = (double *) jl_array_data(tt);
            PROTECT(ans = allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);;
    }
    else if (jl_is_int32(val))
    {
         int32_t *p = (int32_t *) jl_array_data(tt);
         PROTECT(ans = allocArray(INTSXP, dims));\
         for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
         UNPROTECT(1);
    }
    else if (jl_is_int64(val))
    {
        int is_int32 = 1;
        int64_t *p = (int64_t *) jl_array_data(tt);
        for (size_t ii=0;ii<len;ii++)
        {
            if (p[ii]>INT32_MAX || p[ii]<INT32_MIN)
            {
                is_int32 = 0;
                break;
            }
        }
        if (is_int32)
        {
            PROTECT(ans = allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
            PROTECT(ans = allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);
        }
    }
    else if (jl_is_bool(val))
    {
        bool *p = (bool *) jl_array_data(tt);
        PROTECT(ans = allocArray(LGLSXP, dims));
        for (size_t i = 0; i < len; i++)
           LOGICAL(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (jl_is_int8(val))
    {
        int8_t *p = (int8_t *) jl_array_data(tt);
        PROTECT(ans = allocArray(INTSXP, dims));\
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (jl_is_uint8(val))
    {
        uint8_t *p = (uint8_t *) jl_array_data(tt);
        PROTECT(ans = allocArray(INTSXP, dims));\
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (jl_is_int16(val))
    {
        int16_t *p = (int16_t *) jl_array_data(tt);
        PROTECT(ans = allocArray(INTSXP, dims));\
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (jl_is_uint16(val))
    {
        uint16_t *p = (uint16_t *) jl_array_data(tt);
        PROTECT(ans = allocArray(INTSXP, dims));\
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (jl_is_uint32(val))
    {
        int is_int32 = 1;
        uint32_t *p = (uint32_t *) jl_array_data(tt);
        for (size_t ii=0;ii<len;ii++)
        {
            if (p[ii]>INT32_MAX || p[ii]<INT32_MIN)
            {
                is_int32 = 0;
                break;
            }
        }
        if (is_int32)
        {
            PROTECT(ans = allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
            PROTECT(ans = allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);
        }
    }
    else if (jl_is_uint64(val))
    {
        int is_int32 = 1;
        uint64_t *p = (uint64_t *) jl_array_data(tt);
        for (size_t ii=0;ii<len;ii++)
        {
            if (p[ii]>INT32_MAX || p[ii]<INT32_MIN)
            {
                is_int32 = 0;
                break;
            }
        }
        if (is_int32)
        {
            PROTECT(ans = allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
          PROTECT(ans = allocArray(REALSXP, dims));
          for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
          UNPROTECT(1);
        }
    }
    //double
    else if (jl_is_float32(val))
    {
        float *p = (float *) jl_array_data(tt);
        PROTECT(ans = allocArray(REALSXP, dims));
        for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
        UNPROTECT(1);;
    }
    //utf8 string
    else if (jl_is_utf8_string(val))
    {
        PROTECT(ans = allocArray(STRSXP, dims));
        for (size_t i = 0; i < len; i++)
           SET_STRING_ELT(ans, i, mkCharCE(jl_string_data(jl_cellref(tt, i)), CE_UTF8));
       UNPROTECT(1);
    }
    else if (jl_is_ascii_string(val))
    {
        PROTECT(ans = allocArray(STRSXP, dims));
        for (size_t i = 0; i < len; i++)
           SET_STRING_ELT(ans, i, mkChar(jl_string_data(jl_cellref(tt, i))));
       UNPROTECT(1);
    }
    return ans;
}

SEXP jr_range(jl_value_t *tt) {
    SEXP ans = R_NilValue;
    jl_value_t *ret = jl_call1(jl_get_function(jl_base_module, "vcat"), tt);
    ans = jr_array(ret);
    return ans;
}

SEXP jr_data_array(jl_value_t *tt) {
    SEXP ans = R_NilValue;
    jl_function_t *func = jl_get_function(jl_base_module, "getindex");
    jl_value_t *u = jl_get_nth_field(tt, 0);
    jl_value_t *v = jl_get_nth_field(tt, 1);
    JL_GC_PUSH2(&u, &v);
    size_t len = jl_array_len(u);
    ans = jr_array(u);
    int ty = TYPEOF(ans);
    for(size_t i=0; i<len; i++){
        if (jl_unbox_bool(jl_call2(func, v, jl_box_int64((int64_t) i+1)))){
            switch(ty) {
              case LGLSXP:
                LOGICAL(ans)[i] = NA_LOGICAL;
                break;
              case INTSXP:
                INTEGER(ans)[i] = NA_INTEGER;
                break;
              case REALSXP:
                REAL(ans)[i] = NA_REAL;
                break;
              case STRSXP:
                SET_STRING_ELT(ans, i, NA_STRING);
                break;
              default:
                LOGICAL(ans)[i] = NA_LOGICAL;
            }
        }
    }
    JL_GC_POP();
    return ans;
}

SEXP jr_data_frame(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    SEXP rnames, d;
    jl_array_t *names = (jl_array_t *) jl_get_nth_field(jl_get_nth_field(tt, 1), 1);
    jl_array_t *columns = (jl_array_t *) jl_get_nth_field(tt, 0);
    JL_GC_PUSH2(&names, &columns);

    size_t n = jl_array_len(jl_get_nth_field(jl_arrayref(columns, 0), 0));
    size_t m = jl_array_len(columns);
    PROTECT(ans = allocVector(VECSXP, m));
    PROTECT(rnames = allocVector(STRSXP, m));
    for(size_t i=0; i<m; i++)
    {
        SET_VECTOR_ELT(ans, i, jr_data_array((jl_value_t *) jl_arrayref(columns, i)));
        SET_STRING_ELT(rnames, i, mkChar(((jl_sym_t *) jl_arrayref(names, i))->name));
    }
    setAttrib(ans, R_NamesSymbol, rnames);
    setAttrib(ans, R_ClassSymbol, mkString("data.frame"));
    d = PROTECT(allocVector(INTSXP ,n));
    for(size_t i=0; i<n; i++){
        INTEGER(d)[i] = i+1;
    }
    setAttrib(ans, R_RowNamesSymbol, d);
    UNPROTECT(3);
    JL_GC_POP();
    return ans;
}

SEXP jr_dict(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    SEXP rnames;
    jl_function_t *str = jl_get_function(jl_base_module, "string");
    jl_array_t *ctt = (jl_array_t *) jl_call1(jl_get_function(jl_base_module, "collect"), tt);
    size_t m = jl_array_len(ctt);
    PROTECT(rnames = allocVector(STRSXP, m));
    PROTECT(ans = allocVector(VECSXP, m));
    jl_value_t *key;
    for(size_t i=0; i<m; i++)
    {
        SET_VECTOR_ELT(ans, i, jr_wrap(jl_tupleref(jl_arrayref(ctt, i), 1), 0));
        key = jl_call1(str, jl_tupleref(jl_arrayref(ctt, i), 0));
        SET_STRING_ELT(rnames, i, mkChar(jl_string_data(key)));
    }
    setAttrib(ans, R_NamesSymbol, rnames);
    UNPROTECT(2);
    return ans;
}

SEXP jr_wrap(jl_value_t *tt, bool own){
    SEXP ans = R_NilValue;
    JL_GC_PUSH1(&tt);
    if (jl_is_nothing(tt) || jl_is_null(tt))
        return ans;

    if (jl_is_array(tt))
    {
        ans = jr_array(tt);
    }
    else if(jl_isa(tt, "Range"))
    {
        ans = jr_range(tt);
    }
    else if(jl_isa(tt, "DataArray"))
    {
        ans = jr_data_array(tt);
    }
    else if(jl_isa(tt, "DataFrame"))
    {
        ans = jr_data_frame(tt);
    }
    else if (jl_is_tuple(tt))
    {
        PROTECT(ans = allocVector(VECSXP, jl_tuple_len(tt)));
        for (int i = 0; i < jl_tuple_len(tt); i++)
            SET_VECTOR_ELT(ans, i, jr_wrap(jl_tupleref(tt, i), 0));
        UNPROTECT(1);
    }
    else if(jl_isa(tt, "Dict"))
    {
        ans = jr_dict(tt);
    }
    else
    {
        ans = jr_scalar(tt);
    }
    JL_GC_POP();
    if (own)
        R_PreserveObject(ans);
    return ans;
}
