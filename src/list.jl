# list getter

function Base.getindex(x::RList, i)
    ptr = ccall(rsym(:sexp_list_getindex), Ptr{Void}, (Ptr{Void}, Ptr{Void}), x.ptr, convert(RArray, i).ptr)
    _factory(ptr)
end

#TODO: list setter


# RList function

Base.keys(x::RList) = names(x)

# converter
function Base.convert(::Type{DataFrame}, x::RList)
    ptr = ccall(rsym(:rj_data_frame), Ptr{Any}, (Ptr{Void},), x.ptr)
    unsafe_pointer_to_objref(ptr)
end
function Base.convert(::Type{Dict}, x::RList)
    ptr = ccall(rsym(:rj_list), Ptr{Any}, (Ptr{Void},), x.ptr)
    unsafe_pointer_to_objref(ptr)
end
Base.convert(::Type{RList}, x::DataFrame) = jr_cast(x)
Base.convert(::Type{RList}, x::Dict) = jr_cast(x)
