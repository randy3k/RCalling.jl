typealias RVector{T} RArray{T, 1}
typealias RMatrix{T} RArray{T, 2}
typealias RVecOrMat{T} Union(RArray{T, 1}, RArray{T, 2})


# RArray functions

Base.summary(a::RArray) = string(Base.dims2string(size(a)), " ", typeof(a))

Base.start(x::RArray) = 1
Base.endof(x::RArray) = length(x)
Base.done(x::RArray, state) = length(x) == state-1
Base.next(x::RArray, state) = x[state], state+1

# TODO: vcat, hcat etc
