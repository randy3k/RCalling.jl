function packpath(dir::String, name::String)
    return joinpath(Pkg.dir(), "RCalling", dir, name)
end

function packpath(dir::String)
    return joinpath(Pkg.dir(), "RCalling", dir)
end

searchdir(path,key) = filter(x->contains(x,key), readdir(path))

# build the Embedded R
shared_file = packpath("deps", "librcall.so")
deps = packpath("deps")

rebuild = false

if isfile(shared_file)
    cfiles = vcat(searchdir(packpath("deps"), ".c"), searchdir(packpath("deps"), ".h"))
    cd(packpath("deps")) do
        for f in cfiles
            if  mtime(shared_file) < mtime(f)
                rebuild = true
                break
            end
        end
    end
else
    rebuild = true
end

if rebuild
    cd(packpath("deps")) do
        run(`make clean`)
        run(`make`)
    end
end
