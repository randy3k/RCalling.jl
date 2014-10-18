function parse(cmd::ByteString)
    ret = ccall(rsym(:RCall_parse), Ptr{Void}, (Ptr{Void},), RArray(cmd).ptr)
    _factory(ret)
end

function eval(e::RExpression, env::REnvironment)
    ret = ccall(rsym(:RCall_eval), Ptr{Void}, (Ptr{Void},Ptr{Void}), e.ptr, env.ptr)
    _factory(ret)
end

R(cmd::ByteString, env::REnvironment) = eval(parse(cmd), env)
R(cmd::ByteString) = eval(parse(cmd), GlobalEnv())

macro R(cmd)
    Expr(:call, :R, "$cmd")
end

macro Rpush(args...)
    calls = map((x) -> :(rassign($(string(x)), $(esc(x)), GlobalEnv())), args)
    Expr(:block, calls..., :(nothing))
end

macro Rpull(args...)
    calls = map((x) -> :($(esc(x)) = rget($(string(x)), GlobalEnv())), args)
    Expr(:block, calls..., :(nothing))
end
