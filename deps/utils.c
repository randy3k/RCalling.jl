#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <julia.h>

void fixstdio(){
	setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
	setvbuf(stderr, NULL, _IOLBF, BUFSIZ);
}

int jl_isa(jl_value_t* tt, char* type)
{
    jl_value_t* atype = jl_get_global(jl_current_module, jl_symbol(type));
    if ((atype == NULL) || (! jl_is_datatype(atype)))
        return 0;
    return jl_subtype(tt, atype, 1);
}

int r_isa(SEXP ss, const char *name)
{
    SEXP cls = Rf_getAttrib(ss, R_ClassSymbol);
    if (cls == R_NilValue) return 0;
    if (strcmp(CHAR(STRING_ELT(cls, 0)), name)==0) return 1;
    return 0;
}
