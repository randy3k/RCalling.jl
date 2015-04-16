An R interface of Julia.
====
[![Build Status](https://travis-ci.org/randy3k/RCalling.jl.svg?branch=master)](https://travis-ci.org/randy3k/RCall.jl)

It is based one the work of [Rif](https://github.com/lgautier/Rif.jl) and [RJulia](https://github.com/armgong/RJulia). Since the development of Rif has been slowed down and I wanted to learn more about the Julia and R, I started this project.

This package was named `RCall.jl`. However a more promising package [RCall.jl](https://github.com/JuliaStats/RCall.jl) is now alive at JuliaStats, I have renamed this to `RCalling.jl`.

Installation
------

1. Make sure you have Julia and R installed and their locations are in the PATH variable.

2. You are eithing using a binary distribution of Julia or a "make install" distribution.

```
Pkg.clone("https://github.com/randy3k/RCalling.jl.git")
Pkg.build("RCalling")
```

Developed on Mac OSX 10.10, R 3.1.2, Julia 0.3.2


Demo
------
[General usage](http://randylai.me/RCalling.jl)

Credits
------
[lgautier](https://github.com/lgautier) and [armgong](https://github.com/armgong).
