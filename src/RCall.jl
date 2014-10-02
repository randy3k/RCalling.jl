module RCall

# build julia to r interface
include("build.jl")

# initialize r
include("init.jl")
init()

# parse and eval
include("eval.jl")

end
