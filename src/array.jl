# general outer constructor
# RArray{T<:Real}(x::Range{T}) = jr_cast(x)
# RArray{T<:Union(Real, ByteString), N}(x::Array{T, N}) = jr_cast(x)
# RArray(x::Union(Real, ByteString)) = jr_cast(x)
# RArray{T<:Union(Real, ByteString), N}(x::DataArray{T, N}) = jr_cast(x)

typealias RVector{T} RArray{T, 1}
typealias RMatrix{T} RArray{T, 2}
typealias RVecOrMat{T} Union(RArray{T, 1}, RArray{T, 2})


# RArray functions

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
