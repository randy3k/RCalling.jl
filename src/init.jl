function init()
    rhome = rstrip(readall(`R RHOME`))
    ENV["R_HOME"] = rhome
    ret = ccall(rsym(:rcall_init), Int32, ())
    set_global_env()
    set_base_env()
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
    ccall(rsym(:rcall_process_events), Void, ())
    nothing
end
