# dirty functions for printing graphics on ipytho

_tf = Any[]

function dev_on()
    if :IJulia in names(Main)
        tf = @R tempfile(fileext=".png")
        png = rget("png")
        rcall(png, Any[tf])
        append!(_tf, Base.convert(Array, tf))
    else
        error("IJulia only!")
    end
    nothing
end

function dev_off()
    if :IJulia in names(Main)
        @R dev.off()
        tf = pop!(_tf)
        fid = open(tf, "r")
        x = readbytes(fid)
        close(fid)
        display(MIME("image/png"), x)
    else
        error("IJulia only!")
    end
    nothing
end

# TODO: some custom functions for grpahics and ggplot2
