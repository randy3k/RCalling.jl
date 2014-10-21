function Base.pointer{T<:Real, N}(s::RArray{T, N})
    ccall(rsym(:sexp_pointer), Ptr{Void}, (Ptr{Void},), s.ptr)
end

function weak_ref{T, N}(ptype::Type, x::RArray{T, N})
	reshape(pointer_to_array(convert(ptype, pointer(x)), length(x)), size(x))
end

weak_ref{N}(x::RArray{Bool, N}) = weak_ref(Ptr{Int32}, x)
weak_ref{N}(x::RArray{Int32, N}) = weak_ref(Ptr{Int32}, x)
weak_ref{N}(x::RArray{Float64, N}) = weak_ref(Ptr{Float64}, x)

# int32, float64 getter
Base.getindex{T<:Union(Float64, Int32), N}(x::RArray{T, N}, args...) = convert(RArray, getindex(weak_ref(x), args...))

# int32, float64 setter

Base.setindex!{T<:Union(Float64, Int32), N}(x::RArray{T, N}, y::Union(Real, Array), args...) = Base.setindex!(weak_ref(x), y, args...)
Base.setindex!{T<:Union(Float64, Int32), N}(x::RArray{T, N}, y::RArray, args...) = Base.setindex!(x, weak_ref(y), args...)

# bool getter

Base.getindex{N}(x::RArray{Bool, N}, args...) = convert(RArray, bool(getindex(weak_ref(x), args...)))

# bool setter
function Base.setindex!{N}(x::RArray{Bool, N}, y::Union(Bool, Array{Bool}), args...)
    weak_ref(x)[args...] = y
end

Base.setindex!{N}(x::RArray{Bool, N}, y::RArray{Bool, N}, args...) = Base.setindex!(x, rj_cast(y), args...)

# converter r to j

function Base.convert{T<:Real, N}(::Type{Array}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end
function Base.convert{T<:Real, N}(::Type{DataArray}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_data_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end
function Base.convert{T<:Real, N}(::Type{PooledDataArray}, x::RArray{T, N})
	ptr = ccall(rsym(:rj_pooled_data_array), Ptr{Any}, (Ptr{Void},), x.ptr)
	unsafe_pointer_to_objref(ptr)
end

# convert j to r
Base.convert{T<:Real, N}(::Type{RArray}, x::Array{T, N}) = jr_cast(x)
Base.convert{T<:Real, N}(::Type{RArray}, x::DataArray{T, N}) = jr_cast(x)
Base.convert(::Type{RArray}, x::Real) = jr_cast(x)
Base.convert{T<:Real}(::Type{RArray}, x::Range{T}) = jr_cast(x)
