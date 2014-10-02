function init(verbose::Bool = false)
    rhome = rstrip(readall(`R RHOME`))
    if verbose
        print("Using R_HOME=", rhome, "\n")
    end

    ENV["R_HOME"] = rhome
    ret = ccall(rsym(:RCall_init), Int32, ())

    if isinteractive()
        timeout = Timer((x)-> R_ProcessEvents())
        start_timer(timeout, 0.05, 0.05)
    end
    return ret
end


function R_ProcessEvents()
  res = ccall(rsym(:RCall_ProcessEvents), Ptr{Void}, ())
  if res == -1
    error("Error in Process Events")
  end
end
