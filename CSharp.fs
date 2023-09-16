namespace Parser

module CSharp =
    open System
    open System.Text
    open System.IO
    open System.Linq

    let mutable csharpMap =
        [
            "[*]f64", "double*";
            "[*c]const f64", "double[]";
            "[*c]f64", "double*";
            "*f64", "out double";
            "f64", "double";
            "[*]f32", "float*";
            "f32", "float";
            "*c_int", "out int";
            "c_int", "int";
            "*c_uint", "out uint";
            "c_uint", "uint";
            "u32", "uint";
            "u64", "ulong";
            "i64", "long";
            "i32", "int";
            "bool", "bool";
            "isize", "nint";
            "usize", "nint";
            "[*c][*c]const u8", "string[]";
            "[*c]const u8", "string";
            "[*]const u8", "string";
            "[*c]u8", "byte*";
            "[*]u8", "byte*";
            "u8", "byte";
            "*anyopaque", "void*";
            "void", "void";
        ] |> Map.ofList

    let keywords =
        [
            "abstract"
            "as"
            "base"
            "bool"
            "break"
            "byte"
            "case"
            "catch"
            "char"
            "checked"
            "class"
            "const"
            "continue"
            "decimal"
            "default"
            "delegate"
            "do"
            "double"
            "else"
            "enum"
            "event"
            "explicit"
            "extern"
            "false"
            "finally"
            "fixed"
            "float"
            "for"
            "foreach"
            "goto"
            "if"
            "implicit"
            "in"
            "int"
            "interface"
            "internal"
            "is"
            "lock"
            "long"
            "namespace"
            "new"
            "null"
            "object"
            "operator"
            "out"
            "override"
            "params"
            "private"
            "protected"
            "public"
            "readonly"
            "ref"
            "return"
            "sbyte"
            "sealed"
            "short"
            "sizeof"
            "stackalloc"
            "static"
            "string"
            "struct"
            "switch"
            "this"
            "throw"
            "true"
            "try"
            "typeof"
            "uint"
            "ulong"
            "unchecked"
            "unsafe"
            "ushort"
            "using"
            "virtual"
            "void"
            "volatile"
            "while"
            "add"
            "and"
            "alias"
            "ascending"
            "args"
            "async"
            "await"
            "by"
            "descending"
            "dynamic"
            "equals"
            "file"
            "from"
            "get"
            "global"
            "group"
            "init"
            "into"
            "join"
            "let"
            "managed"
            "nameof"
            "nint"
            "not"
            "notnull"
            "nuint"
            "on"
            "or"
            "orderby"
            "partial"
            "partial"
            "record"
            "remove"
            "required"
            "scoped"
            "select"
            "set"
            "unmanaged"
            "unmanaged"
            "value"
            "var"
            "when"
            "where"
            "where"
            "with"
            "yield"
        ]

    let sb = new StringBuilder(10 * 1024)

    let sb_append (str: string) = sb.Append(str) |> ignore
    let sb_appendln (str: string) = sb.AppendLine(str) |> ignore
    let sb_clear() = sb.Clear() |> ignore

    let split (str: string) = 
        let arr = str.Split(':')
        (arr[0].Trim(), arr[1].Trim())


    let convertToCS (name: string) =
        let i = name.IndexOf('*');
        if(i < 0) then name
        else name[i + 1..] + "*"

        
    let create_template (_libname: string) =
        sb_clear()
        "using System;" |> sb_appendln
        "using System.Runtime.InteropServices;" |> sb_appendln
        "using System.Diagnostics;\n" |> sb_appendln

        let libname = _libname |> suffix
        let idx = libname.IndexOf('.')
        let lib = libname[0..idx - 1]
        $"namespace {lib.ToUpper()};\n" |> sb_appendln
        $"public static unsafe class {lib.ToUpper()}\n{{\n" |> sb_appendln  // close the bracket at the end of serialization
        $"    const string libname = \"{libname}\";\n" |> sb_appendln
        sb |> string


    let transform_enum (entry: string) =
        sb_clear()    
        "    public enum " |> sb_append

        let lines = entry.Split '\n'        
        let decl = content lines[0] "const" "="        
        if not (csharpMap.ContainsKey decl) then
            csharpMap <- csharpMap.Add(decl, decl)
        $"{decl}\n    {{" |> sb_appendln
   
        lines 
        |> Seq.skip 1 
        |> Seq.iter (fun line -> if hasletter line then $"        {line.Trim()}" |> upper |> sb_appendln)

        "    }\n" |> sb_appendln
        sb |> string


    let transform_struct (entry: string) =
        sb_clear()
        let mutable size: int = 0
        let lines = entry.Split '\n'        
        let decl =  content lines[0] "const" "="

        lines
        |> Seq.skip 1
        |> Seq.iter (fun line -> 
                        if line |> hasletter then
                            let lhs, rhs = split line
                            size <- size +
                            match typesLenghts.TryFind(rhs[0..^1]) with
                            | Some len -> len
                            | None -> 8) 
        $"    [StructLayout(LayoutKind.Explicit, Size={size})]" |> sb_appendln
        "    public unsafe struct " |> sb_append

        if not (csharpMap.ContainsKey decl) then
            csharpMap <- csharpMap.Add(decl, decl)
            typesLenghts <- typesLenghts.Add(decl, size)
            
        decl |> sb_appendln
        "    {" |> sb_appendln

        
        let mutable offset: int = 0
        for line in lines.Skip(1) do
            if hasletter line then
                let lhs, rhs = split line
                let name = 
                    match csharpMap.TryFind(rhs[0..^1]) with                    
                    | Some(s) -> s        
                    | None -> rhs[0..^1]                
                $"        [FieldOffset({offset})] public {name} {(lhs |> upper)};" |> sb_appendln
                offset <- offset +
                    match typesLenghts.TryFind(rhs[0..^1]) with
                        | Some len -> len
                        | None -> 8
        "    }\n" |> sb_appendln
        sb |> string


    let transform_function (entry: string) = 
        sb_clear()
        "    [DllImport(libname, CharSet=CharSet.Ansi, CallingConvention=CallingConvention.Cdecl)]" |> sb_appendln
        "    public static extern unsafe " |> sb_append

        // write the return type
        let tt = content entry ")" "{"
        let ttname = 
            match csharpMap.TryFind(tt) with
            | Some(s) -> s
            | None -> convertToCS tt  
        ttname |> sb_append

        // write the name
        let fnname = content entry "fn" "("
        $" {fnname}(" |> sb_append

        // write the signature
        let sign = content entry "(" ")"
        if sign |> hasletter then
            let args = sign.Split(',')
            for arg in args do
                if hasletter arg then
                    let lhs, rhs = split arg
                    let name = 
                        match csharpMap.TryFind(rhs) with                    
                        | Some(s) -> s             
                        | None -> rhs
                    let _type = if keywords.Contains(lhs) then "_" + lhs else lhs
                    $"{name} {_type}, " |> sb_append            
            sb.Remove(sb.Length - 2, 2) |> ignore

        ");\n" |> sb_appendln
        sb |> string



  

