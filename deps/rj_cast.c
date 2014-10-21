#define R_NO_REMAP
#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

extern int sexp_is_ascii(SEXP ss);
extern jl_tuple_t *sexp_size(const SEXP s);
extern jl_array_t *sexp_names(const SEXP s);
jl_value_t *rj_cast(SEXP ss);

static inline jl_array_t *ptr_to_array(jl_datatype_t *type, void* data, jl_tuple_t *dims)
{
  return jl_ptr_to_array(jl_apply_array_type(type, jl_tuple_len(dims)), data, dims, 0);
}

static inline jl_array_t *new_array(jl_datatype_t *type, jl_tuple_t *dims)
{
  return jl_new_array(jl_apply_array_type(type, jl_tuple_len(dims)), dims);
}

static inline int list_has_names(SEXP ss)
{
    SEXP name = Rf_getAttrib(ss, R_NamesSymbol);
    SEXP e, v;
    if (name == R_NilValue) return 0;
    int errorOccurred;
    e = PROTECT(Rf_lang2(Rf_install("nchar"), name));
    v = PROTECT(R_tryEval(e, R_GlobalEnv, &errorOccurred));
    UNPROTECT(2);
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

static inline int array_has_na(SEXP ss)
{
    int ty = TYPEOF(ss);
    size_t n = LENGTH(ss);
    switch(ty) {
      case LGLSXP:
        for(size_t i=0; i<n; i++)
            if (LOGICAL(ss)[i] == NA_LOGICAL)
                return 1;
        break;
      case INTSXP:
        for(size_t i=0; i<n; i++)
            if (INTEGER(ss)[i] == NA_INTEGER)
                return 1;
        break;
      case REALSXP:
        for(size_t i=0; i<n; i++){
            if (ISNA(REAL(ss)[i]))
                return 1;
        }
        break;
      case STRSXP:
        for(size_t i=0; i<n; i++)
            if (STRING_ELT(ss, i) == NA_STRING)
                return 1;
        break;
    }
    return 0;
}



static inline int r_isa(SEXP ss, const char *name)
{
    SEXP cls = Rf_getAttrib(ss, R_ClassSymbol);
    if (cls == R_NilValue) return 0;
    if (strcmp(CHAR(STRING_ELT(cls, 0)), name)==0) return 1;
    return 0;
}

// adapted from https://github.com/armgong/RJulia/blob/master/src/R_Julia.c
jl_value_t *rj_array(SEXP ss)
{
    jl_value_t *ret = JL_NULL;
    SEXP levels = Rf_getAttrib(ss, R_LevelsSymbol);
    if (levels != R_NilValue)
    {
        jl_error("Convert to PooledDataArray instead.");
        return ret;
    }
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
            ret = (jl_value_t *) new_array(jl_int32_type, dims);
            JL_GC_PUSH1(&ret);
            int *data = (int *)jl_array_data(ret);
            for (size_t i = 0; i < jl_array_len(ret); i++)
                data[i] = INTEGER(ss)[i];
            JL_GC_POP();
            break;
        }
        case REALSXP:
        {
            ret = (jl_value_t *) new_array(jl_float64_type, dims);
            JL_GC_PUSH1(&ret);
            double *data = (double *)jl_array_data(ret);
            for (size_t i = 0; i < jl_array_len(ret); i++)
                data[i] = REAL(ss)[i];
            JL_GC_POP();
            break;
        }
        case STRSXP:
        {
            int is_ascii = sexp_is_ascii(ss);
            if (is_ascii)
                ret = (jl_value_t *) new_array(jl_ascii_string_type, dims);
            else
                ret = (jl_value_t *) new_array(jl_utf8_string_type, dims);
            JL_GC_PUSH1(&ret);

            jl_value_t **data = jl_array_data(ret);

            for (size_t i = 0; i < jl_array_len(ret); i++)
                if (is_ascii)
                    data[i] = jl_cstr_to_string(CHAR(STRING_ELT(ss, i)));
                else
                    data[i] = jl_cstr_to_string(Rf_translateCharUTF8(STRING_ELT(ss, i)));

            JL_GC_POP();
            break;
        }
        default:
            jl_error("RCall does not know how to convert this R object.");
    }
    return ret;
}


static inline jl_value_t *rj_array_uncheck(SEXP ss)
{
    size_t n = LENGTH(ss);
    jl_array_t *na = new_array(jl_bool_type, jl_tuple1(jl_box_long(n)));
    JL_GC_PUSH1(&na);

    int ty = TYPEOF(ss);
    int isna = 0;
    for(size_t i=0; i<n; i++)
    {
        isna = 0;
        switch(ty) {
          case LGLSXP:
            isna = (LOGICAL(ss)[i] == NA_LOGICAL);
            break;
          case INTSXP:
            isna = (INTEGER(ss)[i] == NA_INTEGER);
            break;
          case REALSXP:
            isna = ISNAN(REAL(ss)[i]);
            break;
          case STRSXP:
            isna = (STRING_ELT(ss, i) == NA_STRING);
            break;
          default:
            isna = 0;
        }
        if (isna)
            jl_arrayset(na, jl_true, i);
        else
            jl_arrayset(na, jl_false, i);
    }
    JL_GC_POP();
    jl_function_t *func = jl_get_function(jl_current_module, "DataArray");
    jl_value_t *ret = jl_call2(func, (jl_value_t *) rj_array(ss), (jl_value_t *) na);
    return ret;
}

jl_value_t *rj_data_array(SEXP ss)
{
    SEXP levels = Rf_getAttrib(ss, R_LevelsSymbol);
    if (levels != R_NilValue)
    {
        jl_error("Convert to PooledDataArray instead.");
        return JL_NULL;
    }
    return rj_array_uncheck(ss);
}


jl_value_t *rj_pooled_data_array(SEXP ss)
{
    jl_value_t *ret = JL_NULL;
    SEXP levels = Rf_getAttrib(ss, R_LevelsSymbol);
    if (levels == R_NilValue)
    {
        jl_error("Expect factor array.");
        return ret;
    }
    jl_value_t *labels = rj_array_uncheck(levels);
    jl_value_t *tt;
    if (array_has_na(ss))
        tt = (jl_value_t *) rj_array_uncheck(ss);
    else
        tt = (jl_value_t *) ptr_to_array(jl_int32_type, INTEGER(ss), sexp_size(ss));
    JL_GC_PUSH2(&tt, &labels);
    jl_value_t *index = jl_call1(jl_eval_string("DataArrays.RefArray"), tt) ;
    ret = jl_call2(jl_get_function(jl_current_module, "PooledDataArray"), index, labels);
    JL_GC_POP();
    return ret;
}

jl_value_t *rj_data_frame(SEXP ss)
{
    jl_value_t *ret = JL_NULL;
    if (TYPEOF(ss) == VECSXP)
    {
        SEXP rownames = PROTECT(Rf_getAttrib(ss, R_RowNamesSymbol));
        int align = 0;
        if (TYPEOF(rownames) == STRSXP)
            align = 1;
        size_t n = LENGTH(ss);
        jl_array_t *columns = new_array(jl_any_type, jl_tuple1(jl_box_long(n+align)));
        jl_array_t *names = sexp_names(ss);
        jl_array_t *sym = new_array(jl_symbol_type, jl_tuple1(jl_box_long(n+align)));
        JL_GC_PUSH4(&columns, &names, &sym, &ret);

        if (align)
        // if row names are string
        {
            jl_arrayset(columns, rj_array(rownames), 0);
            jl_arrayset(sym, (jl_value_t *) jl_symbol("RowID"), 0);
        }
        SEXP ssi;
        for (int i = 0; i < n; i++)
        {
            ssi = VECTOR_ELT(ss, i);
            if (Rf_getAttrib(ssi, R_LevelsSymbol) == R_NilValue)
                jl_arrayset(columns, rj_data_array(ssi), i+align);
            else
                jl_arrayset(columns, rj_pooled_data_array(ssi), i+align);

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
    ret = (jl_value_t *) new_array(ttype, jl_tuple1(jl_box_long(n)));
    JL_GC_PUSH1(&ret);
    if (list_has_names(ss))
    {
        jl_array_t *names = sexp_names(ss);
        for (int i = 0; i < n; i++)
        {
            jl_arrayset((jl_array_t *) ret,
                (jl_value_t *) jl_tuple2(jl_arrayref(names, i), rj_cast(VECTOR_ELT(ss, i))), i);
        }
    }
    else
    {
        for (int i = 0; i < n; i++)
        {
            jl_arrayset((jl_array_t *) ret,
                (jl_value_t *) jl_tuple2(jl_box_long(i+1), rj_cast(VECTOR_ELT(ss, i))), i);
        }
    }
    jl_function_t *func = jl_get_function(jl_base_module, "Dict");
    ret = jl_call1(func, ret);
    JL_GC_POP();
    return ret;
}


jl_value_t *rj_cast(SEXP ss)
{
    jl_value_t *ret = JL_NULL;
    if ((LENGTH(ss)) != 0)
    {
        SEXP levels = Rf_getAttrib(ss, R_LevelsSymbol);
        int ty = TYPEOF(ss);
        if ((ty == INTSXP) && (levels != R_NilValue))
        {
            ret = rj_pooled_data_array(ss);
        }
        else if ((ty == LGLSXP) || (ty == INTSXP) || (ty == REALSXP) || (ty == STRSXP))
        {
            if (array_has_na(ss))
                ret = rj_data_array(ss);
            else
                ret = rj_array(ss);
        }
        else if(ty== VECSXP)
        {
            if (r_isa(ss, "data.frame"))
                ret = rj_data_frame(ss);
            else
                ret = rj_list(ss);
        }
        else
        {
            jl_error("RCall does not know how to convert this R object.");
            ret = JL_NULL;
        }
    }
    return (jl_value_t *) ret;
}
