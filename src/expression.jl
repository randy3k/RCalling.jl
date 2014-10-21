function parse(cmd::ByteString)
    ret = ccall(rsym(:rcall_parse), Ptr{Void}, (Ptr{Void},), convert(RArray, cmd).ptr)
    _factory(ret)
end

function eval(e::RExpression, env::REnvironment)
    ret = ccall(rsym(:rcall_eval), Ptr{Void}, (Ptr{Void},Ptr{Void}), e.ptr, env.ptr)
    _factory(ret)
end

R(cmd::ByteString, env::REnvironment) = eval(parse(cmd), env)
R(cmd::ByteString) = eval(parse(cmd), GlobalEnv)

macro R(cmd)
	if typeof(cmd) <: Expr && cmd.head == :(=)
		rhs = cmd.args[2]
		if typeof(rhs) <: Expr && rhs.head == :block
			error("Use R(...) instead.")
			return nothing
		end
	end
    Expr(:call, :R, string(cmd))
end

macro Rpush(args...)
    calls = map((x) -> :(rassign($(string(x)), $(esc(x)), GlobalEnv)), args)
    Expr(:block, calls..., :(nothing))
end

macro Rpull(args...)
    calls = map((x) -> :($(esc(x)) = rget($(string(x)), GlobalEnv)), args)
    Expr(:block, calls..., :(nothing))
end
