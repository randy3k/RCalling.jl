function is_ascii{T <: ByteString, N}(s::RArray{T, N})
    ccall(rsym(:sexp_is_ascii), Int, (Ptr{Void},), s.ptr)
end

function is_ascii(s::Ptr{Void})
    ccall(rsym(:sexp_is_ascii), Int, (Ptr{Void},), s)
end


# Note:only string vector and matrix are support
# string vector getter

function Base.getindex{T<:ByteString, N}(x::RArray{T, N}, i)
    ptr = ccall(rsym(:sexp_getindex), Ptr{Void}, (Ptr{Void}, Ptr{Void}),
    	x.ptr, pointer_from_objref(i))
    _factory(ptr)
end

# string matrix getter

function Base.getindex{T<:ByteString}(x::RArray{T, 2}, i, j)
    ptr = ccall(rsym(:sexp_getindex2), Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ptr{Void}),
    	x.ptr, pointer_from_objref(i), pointer_from_objref(j))
    _factory(ptr)
end

#TODO: string setter


# convertor

# r to j
function Base.convert{T<:ByteString, N}(::Type{Array}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end
function Base.convert{T<:ByteString, N}(::Type{DataArray}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_data_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end
function Base.convert{T<:ByteString, N}(::Type{PooledDataArray}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_pooled_data_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end

# j to r
Base.convert{T<:ByteString, N}(::Type{RArray}, x::Array{T, N}) = jr_cast(x)
Base.convert{T<:ByteString, N}(::Type{RArray}, x::DataArray{T, N}) = jr_cast(x)
Base.convert(::Type{RArray}, x::ByteString) = jr_cast(x)
