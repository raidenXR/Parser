
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
In terminal run `./Parser [source-file] [libname] [-fs|cs] [destination]` \
i.e. `./Parser "../src_file.zig" "libname.dll" -fs "../directory/"` 

otherwise it can run as \
`./Parser "../src_file.zig"' \
to create a '.html' documentation of the `.zig` source file


TODO: implement a '.html' documentation for the generated bindings file.
