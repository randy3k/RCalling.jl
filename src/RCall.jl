module RCall

using DataFrames: DataFrame, DataArray, PooledDataArray
export DataFrame, DataArray, PooledDataArray

# R objects
export RAny, RArray, RList, RFunction, REnvironment, RExpression, RObject, RNull
export RVector, RMatrix
# R object function
export class, attr, rtypeof
# ipython graphics helper
export dev_on, dev_off
# parser
export @R, R
# misc functions
export rcall, rprint, @rimport, @rusing
# get/setter
export @Rpush, @Rpull, rget, rassign

# initialize r
include("init.jl")

include("sexp.jl")

include("environment.jl")

# parse, eval, expression
include("expression.jl")

include("array.jl")

include("numeric.jl")

include("string.jl")

include("list.jl")

include("function.jl")

include("operator.jl")

# ipython graphics
include("graphics.jl")

# start engine
handler = dlopen(joinpath(Pkg.dir(), "RCall", "deps", "librcall.so"))
# TODO: symbol cache
rsym(s) = dlsym(handler, s)
init()
gui()

end
