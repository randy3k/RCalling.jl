function is_ascii{T <: ByteString, N}(s::RArray{T, N})
    ccall(rsym(:sexp_is_ascii), Int, (Ptr{Void},), s.ptr)
end

function is_ascii(s::Ptr{Void})
    ccall(rsym(:sexp_is_ascii), Int, (Ptr{Void},), s)
end


# Note:only string vector and matrix are support
# string vector getter

function Base.getindex{N}(x::RArray{UTF8String, N}, i)
    ptr = ccall(rsym(:sexp_subset), Ptr{Void}, (Ptr{Void}, Ptr{Void}), x.ptr, convert(RArray, i).ptr)
    _factory(ptr)
end

# string matrix getter

function Base.getindex(x::RArray{UTF8String, 2}, i, j)
    ptr = ccall(rsym(:sexp_subset2), Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ptr{Void}), x.ptr, convert(RArray, i).ptr, convert(RArray, j).ptr)
    _factory(ptr)
end

#TODO: string setter


# convertor

Base.convert{T<:ByteString, N}(::Type{Array}, x::RArray{T,N}) = rj_wrap(x)

Base.convert{T<:ByteString, N}(::Type{RArray}, x::Array{T, N}) = jr_wrap(x)
Base.convert{T<:ByteString, N}(::Type{RArray}, x::DataArray{T, N}) = jr_wrap(x)
Base.convert(::Type{RArray}, x::ByteString) = jr_wrap(x)
