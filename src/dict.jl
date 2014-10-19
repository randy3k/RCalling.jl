RDict(x::DataFrame) = jr_wrap(x)
RDict(x::Dict) = jr_wrap(x)

# list getter

function Base.getindex(x::RDict, i)
    ptr = ccall(rsym(:sexp_listsubset), Ptr{Void}, (Ptr{Void}, Ptr{Void}), x.ptr, RArray(i).ptr)
    _factory(ptr)
end

#TODO: list setter


# RDict function

Base.keys(x::RDict) = names(x)

# print function
function Base.show(io::IO, s::RDict)
    if s.ptr == C_NULL
        return
    end
    try
        c = join(class(s), " ")
        print(io, typeof(s), "<$c>")
    catch
        print(io, typeof(s))
    end

    nothing
end

# converter
Base.convert(::Type{DataFrame}, x::RDict) = Base.convert(DataFrame, rj_wrap(x))
Base.convert(::Type{Dict}, x::RDict) = rj_wrap(x)
Base.convert(::Type{RDict}, x::DataFrame) = jr_wrap(x)
Base.convert(::Type{RDict}, x::Dict) = jr_wrap(x)
