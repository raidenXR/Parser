
A parser that reads a `.zig` source file, and creates bindings in
- C#
- F#

for the `export` functions and `extern` structs and `enums(c_int)` enums that file contains.

It can compile with NativeAoT
`dotnet publish -c Release -r linux-x64`

or simple build with `dotnet build`

P.S
`printfn` and `sprintfn` use reflection, thus are not suitable for AoT compiled bindings.
Avoid those functions and use `Console.WriteLine` and `interpolated-strings` instead.

#### How to use
In terminal run `./Parser [source-file] [libname] [-fs|cs] [destination]`
i.e. `./Parser "../src_file.zig" "libname.dll" -fs "../directory/"` 


TODO: update the src 
TODO: generate simple .html documentation. A table with name definitions - declaration and description
TODO: assign as [<Obsolete>] the old functions


open System

[<Obsolete("Do not use. Use newFunction instead.")>]
let obsoleteFunction x y =
  x + y

let newFunction x y =
  x + 2 * y

// The use of the obsolete function produces a warning.
let result1 = obsoleteFunction 10 100
let result2 = newFunction 10 100
