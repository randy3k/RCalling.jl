# general outer constructor
# RArray{T<:Real}(x::Range{T}) = jr_wrap(x)
# RArray{T<:Union(Real, ByteString), N}(x::Array{T, N}) = jr_wrap(x)
# RArray(x::Union(Real, ByteString)) = jr_wrap(x)
# RArray{T<:Union(Real, ByteString), N}(x::DataArray{T, N}) = jr_wrap(x)

typealias RVector{T} RArray{T, 1}
typealias RMatrix{T} RArray{T, 2}
typealias RVecOrMat{T} Union(RArray{T, 1}, RArray{T, 2})


# int32, float64 getter

Base.getindex{T<:Union(Float64, Int32), N}(x::RArray{T, N}, args...) = convert(RArray, getindex(rj_wrap(x), args...))

# int32, float64 setter

Base.setindex!{T<:Union(Float64, Int32), N}(x::RArray{T, N}, y::Union(Real, Array), args...) = Base.setindex!(rj_wrap(x), y, args...)
Base.setindex!{T<:Union(Float64, Int32), N}(x::RArray{T, N}, y::RArray, args...) = Base.setindex!(x, rj_wrap(y), args...)

# bool getter

Base.getindex{N}(x::RArray{Bool, N}, args...) = convert(RArray, getindex(rj_wrap(x), args...))

# bool setter
# since R bool vectors are 32bit, we have to treat them seperately

function Base.setindex!{N}(x::RArray{Bool, N}, y::Union(Bool, Array{Bool}), args...)
    ptr = Base.convert(Ptr{Int32}, pointer(x))
    x = pointer_to_array(ptr, length(x))
    x[args...] = y
end

Base.setindex!{N}(x::RArray{Bool, N}, y::RArray{Bool, N}, args...) = Base.setindex!(x, rj_wrap(y), args...)

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

# RArray functions

function Base.pointer{T, N}(s::RArray{T, N})
    ccall(rsym(:sexp_pointer), Ptr{Void}, (Ptr{Void},), s.ptr)
end

Base.summary(a::RArray) = string(Base.dims2string(size(a)), " ", typeof(a))

function Base.show{T, N}(io::IO, x::RArray{T, N})
    println(io, summary(x))
    rprint(io, x)
    nothing
end

Base.start(x::RArray) = 1
Base.endof(x::RArray) = length(x)
Base.done(x::RArray, state) = length(x) == state-1
Base.next(x::RArray, state) = x[state], state+1

# TODO: vcat, hcat etc

# converter r to j

Base.convert{N}(::Type{Array}, x::RArray{Bool,N}) = rj_wrap(x)
Base.convert{T<:Union(Int32, Float64), N}(::Type{Array}, x::RArray{T,N}) = deepcopy(rj_wrap(x))
Base.convert{T<:ByteString, N}(::Type{Array}, x::RArray{T,N}) = rj_wrap(x)

# convert j to r
Base.convert{T<:Real, N}(::Type{RArray}, x::Array{T, N}) = jr_wrap(x)
Base.convert{T<:ByteString, N}(::Type{RArray}, x::Array{T, N}) = jr_wrap(x)
Base.convert{T<:Real}(::Type{RArray}, x::Range{T}) = jr_wrap(x)
Base.convert(::Type{RArray}, x::Real) = jr_wrap(x)
Base.convert(::Type{RArray}, x::ByteString) = jr_wrap(x)
