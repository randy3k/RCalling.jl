# Julia call Embedded R interface
handler = dlopen(shared_file)
# TODO: symbol cache
rsym(s) = dlsym(handler, s)

function init(verbose::Bool = false)
    rhome = rstrip(readall(`R RHOME`))
    if verbose
        print("Using R_HOME=", rhome, "\n")
    end

    ENV["R_HOME"] = rhome
    ret = ccall(rsym(:RCall_init), Int32, ())
    return ret
end

function gui()
    if isinteractive()
        timeout = Timer((x)-> ProcessEvents())
        start_timer(timeout, 0.04, 0.04)
    end
    nothing
end

function ProcessEvents()
    ccall(rsym(:RCall_ProcessEvents), Void, ())
    nothing
end
