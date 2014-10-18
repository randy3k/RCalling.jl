# TODO: should be faster to do the addiition in c

+(x::RAny, y::RAny) = rcall(rget("+"), Any[x, y])
-(x::RAny, y::RAny) = rcall(rget("-"), Any[x, y])
*(x::RAny, y::RAny) = rcall(rget("*"), Any[x, y])
/(x::RAny, y::RAny) = rcall(rget("/"), Any[x, y])
^(x::RAny, y::RAny) = rcall(rget("^"), Any[x, y])
