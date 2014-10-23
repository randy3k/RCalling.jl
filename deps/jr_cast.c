#define R_NO_REMAP
#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

SEXP jr_cast(jl_value_t *tt, bool own);

#define in_int32_range(x) x<=INT32_MAX && x>=INT32_MIN

static inline int jl_isa(jl_value_t* tt, char* type)
{
    jl_value_t* atype = jl_get_global(jl_current_module, jl_symbol(type));
    if ((atype == NULL) || (! jl_is_datatype(atype)))
        return 0;
    return jl_subtype(tt, atype, 1);
}

// adapted from https://github.com/armgong/RJulia/blob/master/src/R_Julia.c
SEXP jr_scalar(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    double tmpfloat;
    // float64, int64, int32 are most common, so put them in the front
    if (jl_is_float64(tt))
    {
        PROTECT(ans = Rf_ScalarReal(jl_unbox_float64(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int32(tt))
    {
        PROTECT(ans = Rf_ScalarInteger(jl_unbox_int32(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int64(tt))
    {
        tmpfloat=(double)jl_unbox_int64(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = Rf_ScalarInteger((int32_t)jl_unbox_int64(tt)));
        else
            PROTECT(ans = Rf_ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_bool(tt))
    {
        PROTECT(ans = Rf_ScalarLogical(jl_unbox_bool(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int8(tt))
    {
        PROTECT(ans = Rf_ScalarInteger(jl_unbox_int8(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint8(tt))
    {
        PROTECT(ans = Rf_ScalarInteger(jl_unbox_uint8(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_int16(tt))
    {
        PROTECT(ans = Rf_ScalarInteger(jl_unbox_int16(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint16(tt))
    {
        PROTECT(ans = Rf_ScalarInteger(jl_unbox_uint16(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_uint32(tt))
    {
        tmpfloat=(double)jl_unbox_uint32(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = Rf_ScalarInteger((int32_t)jl_unbox_uint32(tt)));
        else
            PROTECT(ans = Rf_ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_uint64(tt))
    {
        tmpfloat=(double)jl_unbox_int64(tt);
        if (in_int32_range(tmpfloat))
            PROTECT(ans = Rf_ScalarInteger((int32_t)jl_unbox_uint64(tt)));
        else
            PROTECT(ans = Rf_ScalarReal(tmpfloat));
        UNPROTECT(1);
    }
    else if (jl_is_float32(tt))
    {
        PROTECT(ans = Rf_ScalarReal(jl_unbox_float32(tt)));
        UNPROTECT(1);
    }
    else if (jl_is_utf8_string(tt))
    {
        PROTECT(ans = Rf_allocVector(STRSXP, 1));
        SET_STRING_ELT(ans, 0, Rf_mkCharCE(jl_string_data(tt), CE_UTF8));
        UNPROTECT(1);
    }
    else if (jl_is_ascii_string(tt))
    {
        PROTECT(ans = Rf_ScalarString(Rf_mkChar(jl_string_data(tt))));
        UNPROTECT(1);
    }
    return ans;
}

// adapted from https://github.com/armgong/RJulia/blob/master/src/R_Julia.c
SEXP jr_array(jl_value_t *tt)
{
    SEXP ans = R_NilValue;
    //get Julia dims and set R array Dims
    int len = jl_array_len(tt);
    if (len == 0)
        return ans;

    jl_datatype_t *ty = jl_array_eltype(tt);
    int ndims = jl_array_ndims(tt);
    SEXP dims;
    PROTECT(dims = Rf_allocVector(INTSXP, ndims));
    for (size_t i = 0; i < ndims; i++)
        INTEGER(dims)[i] = jl_array_dim(tt, i);
    UNPROTECT(1);

    // again, float64, int32 and int64 are most common
    if (ty == jl_float64_type)
    {
            double *p = (double *) jl_array_data(tt);
            PROTECT(ans = Rf_allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);;
    }
    else if (ty == jl_int32_type)
    {
         int32_t *p = (int32_t *) jl_array_data(tt);
         PROTECT(ans = Rf_allocArray(INTSXP, dims));
         for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
         UNPROTECT(1);
    }
    else if (ty == jl_int64_type)
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
            PROTECT(ans = Rf_allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
            PROTECT(ans = Rf_allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);
        }
    }
    else if (ty == jl_bool_type)
    {
        bool *p = (bool *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(LGLSXP, dims));
        for (size_t i = 0; i < len; i++)
           LOGICAL(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (ty == jl_int8_type)
    {
        int8_t *p = (int8_t *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(INTSXP, dims));
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (ty == jl_uint8_type)
    {
        uint8_t *p = (uint8_t *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(INTSXP, dims));
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (ty == jl_int16_type)
    {
        int16_t *p = (int16_t *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(INTSXP, dims));
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (ty == jl_uint16_type)
    {
        uint16_t *p = (uint16_t *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(INTSXP, dims));
        for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
        UNPROTECT(1);
    }
    else if (ty == jl_uint32_type)
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
            PROTECT(ans = Rf_allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
            PROTECT(ans = Rf_allocArray(REALSXP, dims));
            for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
            UNPROTECT(1);
        }
    }
    else if (ty == jl_uint64_type)
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
            PROTECT(ans = Rf_allocArray(INTSXP, dims));
            for (size_t i = 0; i < len; i++) INTEGER(ans)[i] = p[i];
            UNPROTECT(1);
        }
        else
        {
          PROTECT(ans = Rf_allocArray(REALSXP, dims));
          for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
          UNPROTECT(1);
        }
    }
    //double
    else if (ty == jl_float32_type)
    {
        float *p = (float *) jl_array_data(tt);
        PROTECT(ans = Rf_allocArray(REALSXP, dims));
        for (size_t i = 0; i < len; i++) REAL(ans)[i] = p[i];
        UNPROTECT(1);;
    }
    //utf8 string
    else if (ty == jl_utf8_string_type)
    {
        PROTECT(ans = Rf_allocArray(STRSXP, dims));
        for (size_t i = 0; i < len; i++)
           SET_STRING_ELT(ans, i, Rf_mkCharCE(jl_string_data(jl_cellref(tt, i)), CE_UTF8));
       UNPROTECT(1);
    }
    else if (ty == jl_ascii_string_type)
    {
        PROTECT(ans = Rf_allocArray(STRSXP, dims));
        for (size_t i = 0; i < len; i++)
           SET_STRING_ELT(ans, i, Rf_mkChar(jl_string_data(jl_cellref(tt, i))));
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
        if (jl_unbox_bool(jl_call2(func, v, jl_box_long(i+1)))){
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
    PROTECT(ans = Rf_allocVector(VECSXP, m));
    PROTECT(rnames = Rf_allocVector(STRSXP, m));
    for(size_t i=0; i<m; i++)
    {
        SET_VECTOR_ELT(ans, i, jr_data_array((jl_value_t *) jl_arrayref(columns, i)));
        SET_STRING_ELT(rnames, i, Rf_mkChar(((jl_sym_t *) jl_arrayref(names, i))->name));
    }
    Rf_setAttrib(ans, R_NamesSymbol, rnames);
    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("data.frame"));
    d = PROTECT(Rf_allocVector(INTSXP ,n));
    for(size_t i=0; i<n; i++){
        INTEGER(d)[i] = i+1;
    }
    Rf_setAttrib(ans, R_RowNamesSymbol, d);
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
    PROTECT(rnames = Rf_allocVector(STRSXP, m));
    PROTECT(ans = Rf_allocVector(VECSXP, m));
    jl_value_t *key;
    for(size_t i=0; i<m; i++)
    {
        SET_VECTOR_ELT(ans, i, jr_cast(jl_tupleref(jl_arrayref(ctt, i), 1), 0));
        key = jl_call1(str, jl_tupleref(jl_arrayref(ctt, i), 0));
        SET_STRING_ELT(rnames, i, Rf_mkChar(jl_string_data(key)));
    }
    Rf_setAttrib(ans, R_NamesSymbol, rnames);
    UNPROTECT(2);
    return ans;
}

SEXP jr_cast(jl_value_t *tt, bool own){
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
        PROTECT(ans = Rf_allocVector(VECSXP, jl_tuple_len(tt)));
        for (int i = 0; i < jl_tuple_len(tt); i++)
            SET_VECTOR_ELT(ans, i, jr_cast(jl_tupleref(tt, i), 0));
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
    if (ans == R_NilValue)
    {
        jl_error("invaild object");
    }
    if (own)
        // TODO: possible to use other methods to protect ans? Preserve and Release
        R_PreserveObject(ans);
    return ans;
}
