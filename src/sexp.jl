# container of all R objects
abstract RAny

# R objects
for t in (:(RArray{T, N}), :RDict, :RFunction, :REnvironment, :RExpression, :RFunctionCall, :RObject, :RNull)
    @eval begin
        type $t <: RAny
            ptr::Ptr{Void}
        end
    end
end

# wrapper of julia objects
function jr_wrap(x, own::Bool=true)
    ptr = ccall(rsym(:jr_wrap), Ptr{Void}, (Ptr{Void}, Bool), pointer_from_objref(x), own)
    _factory(ptr)
end

# wrapper of julia objects
function rj_wrap(x::RAny)
    ptr = ccall(rsym(:rj_wrap), Ptr{Any}, (Ptr{Void},), x.ptr)
    unsafe_pointer_to_objref(ptr)
end

# print function
function Base.show(io::IO, s::RAny)
    if s.ptr == C_NULL
        return
    end
    print(io, typeof(s))
    nothing
end

function rprint(s::RAny)
    ccall(rsym(:sexp_print), Void, (Ptr{Void},), s.ptr)
    nothing
end

# general RAny functions


function Base.length(s::RAny)
    ccall(rsym(:sexp_length), Int, (Ptr{Void},), s.ptr)
end

function Base.ndims(s::RAny)
    ccall(rsym(:sexp_ndims), Int, (Ptr{Void},), s.ptr)
end

function Base.size(s::RAny)
    ret = ccall(rsym(:sexp_size), Ptr{Void}, (Ptr{Void},), s.ptr)
    obj = _factory(ret)
    if obj == None
        return (1,)
    else
        return tuple(int64(deepcopy(rj_wrap(obj)))...)
    end
end

function Base.size(s::RAny, i::Int64)
    ret = ccall(rsym(:sexp_size), Ptr{Void}, (Ptr{Void},), s.ptr)
    rj_wrap(_factory(ret))[i]
end

function Base.names(s::RAny)
    ret = ccall(rsym(:sexp_names), Ptr{Void}, (Ptr{Void},), s.ptr)
    Base.convert(Array, _factory(ret))
end

function named(s::RAny)
    ccall(rsym(:sexp_named), Int, (Ptr{Void},), s.ptr)
end

function rtypeof(s::RAny)
    ccall(rsym(:sexp_typeof), Int, (Ptr{Void},), s.ptr)
end

function attr(s::RAny, name::ASCIIString)
    # TODO: show all attributes when name is missing
    ret =  ccall(rsym(:sexp_get_attr), Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), s.ptr, name)
    if ret == C_NULL
        error("No such attribute: ", name)
    end
    _factory(ret)
end

function class(s::RAny)
    class_fun = rget("class", GlobalEnv())
    Base.convert(Array, rcall(class_fun, Any[s]))
end


# RAny factory constructor


# frome Rinternals.h
const NILSXP   = 0;  const SYMSXP     = 1;  const LISTSXP    = 2;
const CLOSXP   = 3;  const ENVSXP     = 4;  const PROMSXP    = 5;
const LANGSXP  = 6;  const SPECIALSXP = 7;  const BUILTINSXP = 8;
const LGLSXP   = 10; const INTSXP     = 13; const REALSXP    = 14;
const CPLXSXP  = 15; const STRSXP     = 16; const DOTSXP     = 17;
const ANYSXP   = 18; const VECSXP     = 19; const EXPRSXP    = 20;
const BCODESXP = 21; const EXTPTRSXP  = 22; const WEAKREFSXP = 23;
const RAWSXP   = 24; const S4SXP      = 25; const FUNSXP     = 99;

# reverse map
const SexpType = Dict([
    (0, :NILSXP),   (1, :SYMSXP),     (2, :LISTSXP),
    (3, :CLOSXP),   (4, :ENVSXP),     (5, :PROMSXP),
    (6, :LANGSXP),  (7, :SPECIALSXP), (8, :BUILTINSXP),
    (10, :LGLSXP),   (13, :INTSXP),     (14, :REALSXP),
    (15, :CPLXSXP),  (16, :STRSXP),     (17, :DOTSXP),
    (18, :ANYSXP),   (19, :VECSXP),     (20, :EXPRSXP),
    (21, :BCODESXP), (22, :EXTPTRSXP),  (23, :WEAKREFSXP),
    (24, :RAWSXP),   (25, :S4SXP),      (99, :FUNSXP)
])


# TODO: UTF8String should be something ASCIIString

const SexpType_to_JType = Dict([
    (LGLSXP, Bool),
    (INTSXP, Int32),
    (REALSXP, Float64),
    (STRSXP, UTF8String),
    (VECSXP, RAny)
])

function release_object(s::RAny)
    ccall(rsym(:R_ReleaseObject), Void, (Ptr{Void},), s.ptr)
end

function _factory(ptr::Ptr{Void}, own::Bool=true)
    if ptr == C_NULL
        return None
    end
    t = ccall(rsym(:sexp_typeof), Int32, (Ptr{Void},), ptr)

    if t in (LGLSXP, INTSXP, REALSXP, STRSXP)
        N = ccall(rsym(:sexp_ndims), Int, (Ptr{Void},), ptr)
        T = SexpType_to_JType[t]
        obj = RArray{T, N}(ptr)
    elseif t == VECSXP
        obj = RDict(ptr)
    elseif t in (CLOSXP, BUILTINSXP, SPECIALSXP)
        obj = RFunction(ptr)
    elseif t == ENVSXP
        obj = REnvironment(ptr)
    elseif t == EXPRSXP
        obj = RExpression(ptr)
    elseif t == LANGSXP
        obj = RFunctionCall(ptr)
    elseif t == NILSXP
        return RNull(ptr)
    else
        obj = RObject(ptr)
    end
    if own
        finalizer(obj, release_object)
    end
    obj
end

function Base.convert(::Type{RAny}, x)
    t = typeof(x)
    if t <: Array
        return RArray(x)
    elseif t <: Range
        return RArray(x)
    elseif t <: Real
        return RArray(x)
    elseif t <: ByteString
        return RArray(x)
    elseif t <: Function
        return RFunction(x)
    elseif t <: DataArray
        return RArray(x)
    elseif t<: DataFrame
        return RDict(x)
    elseif t<: Dict
        return RDict(x)
    else
        return x
    end
end

Base.convert(::Type{Any}, x::RNull) = None
