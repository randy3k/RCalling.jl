# outer constructor
# RFunction(f::Function) = Base.convert(RFunction, f)

function rcall(f::RFunction, argv::Vector, argn::Vector{ASCIIString}, env::REnvironment)
    ret = ccall(rsym(:rcall), Ptr{Void},
                  (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                  f.ptr, pointer_from_objref(argv), pointer_from_objref(argn), env.ptr
    )
    _factory(ret)
end

rcall(f::RFunction, argv::Vector, argn::Vector{ASCIIString}) = rcall(f, argv, argn, GlobalEnv)
rcall(f::RFunction, argv::Vector, env::REnvironment) = rcall(f, argv, ["" for i in 1:length(argv)], env)
rcall(f::RFunction, argv::Vector) = rcall(f, argv, ["" for i in 1:length(argv)], GlobalEnv)
rcall(f::RFunction) = rcall(f, [], ASCIIString[], GlobalEnv)

function Base.convert(::Type{Function}, f::RFunction)
    function fn(args...; kwargs...)
        argv_unnamed = Any[x for x in args]
        argn_unnamed = ASCIIString["" for x in args]
        argv_named = Any[x[2] for x in kwargs]
        argn_named = ASCIIString[string(x[1]) for x in kwargs]
        ret = rcall(f, vcat(argv_unnamed, argv_named), vcat(argn_unnamed, argn_named))
        return ret
    end
end

# convert

function Base.convert(::Type{RFunction}, f::Function)
    ret = ccall(rsym(:jr_func), Ptr{Void}, (Ptr{Void},), pointer_from_objref(f))
    _factory(ret)
end
