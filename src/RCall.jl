module RCall

using Reexport
@reexport using DataFrames

# R objects
export RAny, RArray, RList, RFunction, REnvironment, RExpression, RFunctionCall, RObject, RNull
export RVector, RMatrix
# R object function
export class, attr
# ipython graphics helper
export dev_on, dev_off
# parser
export @R, R
# misc functions
export rcall, rprint, @rimport, rtypeof
# get/setter
export @Rpush, @Rpull, rget, rassign

# build julia to r interface
include("build.jl")

# initialize r
include("init.jl")
init()
gui()

include("sexp.jl")

include("environment.jl")

# parse, eval, expression
include("expression.jl")

include("array.jl")

include("list.jl")

include("function.jl")

include("operator.jl")

# ipython graphics
include("graphics.jl")

end
