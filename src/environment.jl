GlobalEnv = None
BaseEnv = None

function set_global_env()
    global GlobalEnv
    GlobalEnv =  _factory(ccall(rsym(:rcall_global_env), Ptr{Void}, ()))
end

function set_base_env()
    global BaseEnv
    BaseEnv = _factory(ccall(rsym(:rcall_base_env), Ptr{Void}, ()))
end

function Base.getindex(x::REnvironment, i::ASCIIString)
    ptr = ccall(rsym(:sexp_list_getindex), Ptr{Void}, (Ptr{Void}, Ptr{Void}), x.ptr, convert(RArray, i).ptr)
    _factory(ptr)
end

function Base.keys(x::REnvironment)
    ls = rget("ls", GlobalEnv)
    convert(Array, rcall(ls, Any[x]))
end

function rget(x::ASCIIString, env::REnvironment)
    ptr = ccall(rsym(:rcall_findVar), Ptr{Void}, (Ptr{Uint8},Ptr{Void}), pointer(x.data), env.ptr)
    obj = _factory(ptr)
    if rtypeof(obj) == 0
        error("Cannot get object.")
    end
    return obj
end
rget(x::ASCIIString) = rget(x, GlobalEnv)

function rassign(name::ASCIIString, x::RAny, env::REnvironment)
    assign_fun = rget("assign", GlobalEnv)
    rcall(assign_fun, Any[convert(RArray, name), x, env], ["", "", "env"])
    nothing
end
rassign(name::ASCIIString, x::RAny) = rassign(name, x, GlobalEnv)

function get_package(x::ASCIIString)
    as_environment = rget("as.environment", GlobalEnv)
    rcall(as_environment, Any[convert(RArray, "package:$x")])
end

# import
const julia_reserved = Set{ASCIIString}((
    "while", "if", "for", "try", "return", "break", "continue", "function",
    "macro", "quote", "let", "local", "global", "const", "abstract",
    "typealias", "type", "bitstype", "immutable", "ccall", "do", "module",
    "baremodule", "using", "import", "export", "importall", "false", "true",
    "__rget__", "__package__")
)

function rlibrary_wrap(x::ASCIIString, s::Symbol)
    R("library($x)")
    pkg = get_package(x)
    members = map((k) ->
        begin
            robj = pkg[k]
            typeof(robj) <: RFunction ? (k, convert(Function, robj)) : (k, robj)
        end,
        keys(pkg)
    )
    filter!(m -> !(m[1] in julia_reserved), members)
    m = Module(s)
    consts = [Expr(:const, Expr(:(=), symbol(x[1]), x[2])) for x in members]
    identity = Expr(:(=), :__package__, x)
    exports = [symbol(x[1]) for x in members]
    eval(m, Expr(:toplevel, consts..., :(__rget__(s) = getindex($(pkg), s)), Expr(:export, exports...), identity))
    m
end

macro rimport(x, args...)
    pkgname = string(x)
    if length(args)==2 && args[1] == :as
        m = args[2]
    elseif length(args)==0
        m = x
    else
        throw(ArgumentError("invalid import syntax."))
    end
    symbol = Expr(:quote, m)
    quote
        if !isdefined($symbol)
            const $(esc(m)) = rlibrary_wrap($pkgname, $symbol)
            nothing
        elseif typeof($(esc(m))) <: Module && :__package__ in names($(esc(m)), true) && $(esc(m)).__package__ == $pkgname
            nothing
        else
            error("$($symbol) already exists!")
            nothing
        end
    end
end

macro rusing(x)
    symbol = Expr(:quote, x)
    quote
        @rimport $(esc(x))
        Main.eval(Expr(:using, $symbol))
    end
end
