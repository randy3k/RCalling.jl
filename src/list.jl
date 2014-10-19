RList(x::DataFrame) = jr_wrap(x)
RList(x::Dict) = jr_wrap(x)

# list getter

function Base.getindex(x::RList, i)
    ptr = ccall(rsym(:sexp_listsubset), Ptr{Void}, (Ptr{Void}, Ptr{Void}), x.ptr, RArray(i).ptr)
    _factory(ptr)
end

#TODO: list setter


# RList function

Base.keys(x::RList) = names(x)

# print function
function Base.show(io::IO, s::RList)
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
Base.convert(::Type{DataFrame}, x::RList) = Base.convert(DataFrame, rj_wrap(x))
Base.convert(::Type{Dict}, x::RList) = rj_wrap(x)
Base.convert(::Type{RList}, x::DataFrame) = jr_wrap(x)
Base.convert(::Type{RList}, x::Dict) = jr_wrap(x)
