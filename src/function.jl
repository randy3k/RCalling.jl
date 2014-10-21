# outer constructor
# RFunction(f::Function) = Base.convert(RFunction, f)

function rcall(f::RFunction, argv::Vector, argn::Vector{ASCIIString}, env::REnvironment)
    n::Int32 = length(argv)
    argv_p = map((x)->x.ptr, argv)
    argn_p = map((x)->pointer(x.data), argn)
    ret = ccall(rsym(:rcall_call), Ptr{Void},
                  (Ptr{Void}, Ptr{Ptr{Void}}, Int32, Ptr{Ptr{Uint8}}, Ptr{Void}),
                  f.ptr, argv_p, n, argn_p, env.ptr
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

        # TOOD: it should be more efficient to do conversions in c
        argv = vcat([typeof(a) <: RAny ? a : convert(RAny, a) for a in argv_unnamed],
            [typeof(a) <: RAny ? a : convert(RAny, a) for a in argv_named])

        ret = rcall(f, argv, vcat(argn_unnamed, argn_named))
        return ret
    end
end

# convert

function Base.convert(::Type{RFunction}, f::Function)
    ret = ccall(rsym(:jr_func_wrap), Ptr{Void}, (Ptr{Void},), pointer_from_objref(f))
    _factory(ret)
end
