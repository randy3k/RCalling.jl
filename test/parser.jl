x = @R x=1;
y = R("y=2");
@test convert(Array,R("x==y"))[1] == false
