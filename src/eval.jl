function eval(cmd::ASCIIString)
    ccall(rsym(:RCall_eval), Ptr{Void}, (Ptr{Uint8},), cmd)
end
