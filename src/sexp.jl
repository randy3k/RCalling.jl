# container of all R objects
abstract RAny

# R objects
for t in (:(RArray{T, N}), :RList, :RFunction, :REnvironment, :RExpression, :RFunctionCall, :RObject, :RNull)
    @eval begin
        type $t <: RAny
            ptr::Ptr{Void}
        end
    end
end

# julia objects cast to r objects
function jr_cast(x, own::Bool=true)
    ptr = ccall(rsym(:jr_cast), Ptr{Void}, (Ptr{Void},), pointer_from_objref(x))
    _factory(ptr, own)
end

# r objects cast to julia objects
function rj_cast(x::RAny)
    ptr = ccall(rsym(:rj_cast), Ptr{Any}, (Ptr{Void},), x.ptr)
    unsafe_pointer_to_objref(ptr)
end

# print function
# Base.show(io::IO, s::RAny) = print(io, typeof(s))

# print function
function Base.show(io::IO, s::RAny)
    if s.ptr == C_NULL
        return
    end
    # TODO: redirect rprint
    oldout = STDOUT
    (rd,wr) = redirect_stdout()
    start_reading(rd)
    ccall(rsym(:sexp_print), Void, (Ptr{Void},), s.ptr)
    flush_cstdio()
    redirect_stdout(oldout)
    close(wr)
    print(io, rstrip(readall(rd)))
    close(rd)
    nothing
end
rprint(s::RAny) = rprint(STDOUT, s)

# general RAny functions


function Base.length(s::RAny)
    ccall(rsym(:sexp_length), Int, (Ptr{Void},), s.ptr)
end

function Base.ndims(s::RAny)
    ccall(rsym(:sexp_ndims), Int, (Ptr{Void},), s.ptr)
end

function Base.size(s::RAny)
    ret = ccall(rsym(:sexp_size), Ptr{Void}, (Ptr{Void},), s.ptr)
    unsafe_pointer_to_objref(ret)
end

function Base.size(s::RAny, i::Int64)
    ret = ccall(rsym(:sexp_size), Ptr{Void}, (Ptr{Void},), s.ptr)
    unsafe_pointer_to_objref(ret)[i]
end

function Base.names(s::RAny)
    ret = ccall(rsym(:sexp_names), Ptr{Void}, (Ptr{Void},), s.ptr)
    unsafe_pointer_to_objref(ret)
end

function named(s::RAny)
    ccall(rsym(:sexp_named), Int, (Ptr{Void},), s.ptr)
end

function marked(s::RAny)
    ccall(RCall.rsym(:sexp_mark), Int, (Ptr{Void},), s.ptr)
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
    class_fun = rget("class", GlobalEnv)
    Base.convert(Array, rcall(class_fun, Any[s]))
end


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


const SexpType_to_JType = Dict([
    (:LGLSXP, Bool),
    (:INTSXP, Int32),
    (:REALSXP, Float64)
])

function release_object(s::RAny)
    ccall(rsym(:R_ReleaseObject), Void, (Ptr{Void},), s.ptr)
end

function preserve_object(s::RAny)
    ccall(rsym(:R_PreserveObject), Void, (Ptr{Void},), s.ptr)
end


function _factory(ptr::Ptr{Void}, own::Bool=true)
    if ptr == C_NULL
        return None
    end
    t = SexpType[ccall(rsym(:sexp_typeof), Int32, (Ptr{Void},), ptr)]

    if t in (:LGLSXP, :INTSXP, :REALSXP, :STRSXP)
        N = ccall(rsym(:sexp_ndims), Int, (Ptr{Void},), ptr)

        if t == :STRSXP
            if is_ascii(ptr) == 1
                T = ASCIIString
            else
                T = UTF8String
            end
        else
            T = SexpType_to_JType[t]
        end
        obj = RArray{T, N}(ptr)
    elseif t == :VECSXP
        obj = RList(ptr)
    elseif t in (:CLOSXP, :BUILTINSXP, :SPECIALSXP)
        obj = RFunction(ptr)
    elseif t == :ENVSXP
        obj = REnvironment(ptr)
    elseif t == :EXPRSXP
        obj = RExpression(ptr)
    elseif t == :LANGSXP
        obj = RFunctionCall(ptr)
    elseif t == :NILSXP
        return RNull(ptr)
    else
        obj = RObject(ptr)
    end
    if own
        preserve_object(obj)
        finalizer(obj, release_object)
    end
    obj
end

function Base.convert(::Type{RAny}, x)
    t = typeof(x)
    if t <: Array
        return convert(RArray, x)
    elseif t <: Range
        return convert(RArray, x)
    elseif t <: Real
        return convert(RArray, x)
    elseif t <: ByteString
        return convert(RArray, x)
    elseif t <: Function
        return convert(RFunction, x)
    elseif t <: DataArray
        return convert(RArray, x)
    elseif t<: DataFrame
        return convert(RList, x)
    elseif t<: Dict
        return convert(RList, x)
    else
        error("RCall does not know how to covert it.")
        return x
    end
end
