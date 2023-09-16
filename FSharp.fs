namespace Parser

module FSharp =
    open System
    open System.Text
    open System.IO
    open System.Linq

    let mutable fsharpMap =
        [
            "[*]f64", "double*";
            "[*c]const f64", "double[]";
            "[*c]f64", "double*";
            "*f64", "double&";
            "f64", "double";
            "[*]f32", "float32*";
            "f32", "float32";
            "*c_int", "int&";
            "c_int", "int";
            "*c_uint", "uint&";
            "c_uint", "uint";
            "u32", "uint";
            "u64", "uint64";
            "i64", "int64";
            "i32", "int";
            "bool", "bool";
            "isize", "nativeint";
            "usize", "nativeint";
            "[*c][*c]const u8", "string[]";
            "[*c]const u8", "string";
            "[*]const u8", "string";
            "[*c]u8", "byte*";
            "[*]u8", "byte*";
            "u8", "byte";
            "*anyopaque", "nativeint";
            "void", "void";
        ] |> Map.ofList

    let keywords = 
        [
            "abstract"
            "and"
            "as"
            "assert"
            "base"
            "class"
            "default"
            "delegate"
            "do"
            "done"
            "downcast"
            "downto"
            "elif"
            "else"
            "end"
            "exception"
            "extern"
            "false"
            "finally"
            "fixed"
            "for"
            "fun"
            "function"
            "global"
            "if"
            "in"
            "inherit"
            "inline"
            "interface"
            "internal"
            "lazy"
            "let"
            "let!"
            "match"
            "match!"
            "member"
            "module"
            "mutable"
            "namespace"
            "new"
            "not"
            "null"
            "open"
            "or"
            "override"
            "private"
            "public"
            "rec"
            "return"
            "return!"
            "select"
            "static"
            "struct"
            "to"
            "true"
            "try"
            "type"
            "upcast"
            "use"
            "use!"
            "val"
            "void"
            "when"
            "while"
            "with"
            "yield"
            "yield!"
            "const"
        ]
        
    let sb = new StringBuilder(10 * 1024)

    let sb_append (str: string) = sb.Append(str) |> ignore
    let sb_appendln (str: string) = sb.AppendLine(str) |> ignore
    let sb_clear() = sb.Clear() |> ignore

    let split (str: string) = 
        let arr = str.Split(':')
        (arr[0].Trim(), arr[1].Trim())


    let convertToFS (name: string) =
        let i = name.IndexOf('*');
        if(i < 0) then name
        else name[i + 1..] + "*"


    let create_template (_libname: string) =
        sb_clear()
        let libname = _libname |> suffix
        let idx = libname.IndexOf('.')
        let lib = if(idx > 0) then libname[0..idx - 1] else libname 

        $"module {lib |> upper} " |> sb_appendln
        // $"module MKXK " |> sb_appendln
        "    open System" |> sb_appendln
        "    open System.Runtime.InteropServices" |> sb_appendln
        "    open System.Diagnostics\n" |> sb_appendln
        "    [<Literal>]" |> sb_appendln
        $"    let libname = \"./{libname}\"\n" |> sb_appendln
        sb |> string

    let transform_enum (entry: string) =
        sb_clear()
        let lines = entry.Split '\n'
        let decl = content lines[0] "const" "="        
        if not (fsharpMap.ContainsKey decl) then
            fsharpMap <- fsharpMap.Add(decl, decl)
        
        // append declaration of enum
        $"    type {decl} =" |> sb_appendln

        let mutable value: int = 0
        lines
        |> Seq.skip 1
        |> Seq.iter (fun line -> 
                        if hasletter line then
                            let name = line.Trim().TrimEnd(',') |> upper
                            $"        | {name} = {value}" |> sb_appendln
                            value <- value + 1)
                             

        "\n" |> sb_append
        sb |> string


    let transform_struct (entry: string) =
        sb_clear()
        let mutable size: int = 0
        let lines = entry.Split '\n'        
        let decl = content lines[0] "const" "="

        lines
        |> Seq.skip 1
        |> Seq.iter (fun line -> 
                        if line |> hasletter then
                            let lhs, rhs = split line
                            size <- size +
                            match typesLenghts.TryFind(rhs[0..^1]) with
                            | Some len -> len
                            | None -> 8) 
        $"    [<Struct; StructLayout(LayoutKind.Explicit, Size={size})>]" |> sb_appendln
        $"    type {decl} = {{" |> sb_appendln

        if not (fsharpMap.ContainsKey decl) then
            fsharpMap <- fsharpMap.Add(decl, decl)
            typesLenghts <- typesLenghts.Add(decl, size)

        let mutable offset: int = 0
        lines
        |> Seq.skip 1
        |> Seq.iter (fun line -> 
                        if line |> hasletter then
                            let lhs, rhs = split line
                            let name = 
                                match fsharpMap.TryFind(rhs[0..^1]) with 
                                |Some s -> s
                                | None -> rhs[0..^1]
                            // let _upper = lhs |> upper 
                            // offset (lhs |> upper) name
                            $"        [<FieldOffset({offset})>] {(lhs |> upper)}: {name}" |> sb_appendln
                            offset <- offset +
                                match typesLenghts.TryFind(rhs[0..^1]) with
                                    | Some len -> len
                                    | None -> 8
                        )
        "    }\n" |> sb_appendln        
        sb |> string

    /// Not sure if it should make use of `csharpMap` or `fsharpMap`
    let transform_function (entry: string) =
        sb_clear()
        "    [<DllImport(libname, CharSet=CharSet.Ansi, CallingConvention=CallingConvention.Cdecl)>]" |> sb_appendln
        "    extern " |> sb_append

        // write the return type
        let tt = content entry ")" "{"
        let ttname = 
            match fsharpMap.TryFind(tt) with
            | Some(s) -> s
            | None -> convertToFS tt    
        ttname |> sb_append

        // write the name
        let fnname = content entry "fn" "("
        $" {fnname}(" |> sb_append

        // write the signature
        let sign = content entry "(" ")"
        if sign |> hasletter then
            let args = sign.Split(',')
            for arg in args do
                if arg |> hasletter then
                    let lhs, rhs = split arg
                    let name = 
                        match fsharpMap.TryFind(rhs) with                    
                        | Some(s) -> s             
                        | None -> rhs
                    let _type = if keywords.Contains(lhs) then "_" + lhs else lhs
                    $"{name} {_type}, " |> sb_append            
            sb.Remove(sb.Length - 2, 2) |> ignore

        ");\n" |> sb_appendln
        sb |> string


    let serialize f s =
        s transform_enum >> transform_struct >> transform_struct