module RCall

import DataFrames: DataArray, DataFrame

# R objects
export RAny, RArray, RDict, RFunction, REnvironment, RExpression, RFunctionCall, RObject, RNull
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

# sexp type definitions
include("sexp.jl")

# environment
include("environment.jl")

# parse, eval, expression
include("expression.jl")

# vector and matrix
include("array.jl")

# list
include("dict.jl")

# function type
include("function.jl")

# function type
include("operator.jl")

# ipython graphics
include("graphics.jl")

end
