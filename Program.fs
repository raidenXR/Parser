open System
open System.IO
open System.Text
open System.Linq
open Parser


let toBuffer (str: string) = 
    let encoder = new UTF8Encoding()
    encoder.GetBytes(str.ToCharArray())
    

let serialize f (s: string) (file: FileStream) =
    s |> f |> toBuffer |> file.Write


let parse_file() =
    let args = Environment.GetCommandLineArgs()
    if args.Length < 5 then
        stdout "invalid arguments\n[filepath] [libname] [type: -fs|-cs] [destination]"
        Environment.Exit(0)
    
    let src = args[1]
    let libname = args[2]
    let generator = args[3]
    let prefix = args[4]

    Console.WriteLine($"generator: {generator}")

    if(src.Contains(".zig")) then do
        let enums_seq = enums src
        let structs_seq = structs src
        let fns_seq = functions src
        let enums_c, structs_c, fns_c = (enums_seq.Count(), structs_seq.Count(), fns_seq.Count())
        stdout $"{enums_c}, {structs_c}, {fns_c}"

        let filename = 
            match generator with
            | "-fs" -> src[0..^4] + ".fs"
            | "-cs" -> src[0..^4] + ".cs"
            | _ -> raise (invalidArg "-generator" $"invalid type {generator}")

    
        use file = File.Create(prefix + (filename |> suffix))
        match generator with
        | "-fs" -> do 
            FSharp.create_template(libname) |> toBuffer |> file.Write
            enums_seq |> Seq.iter (fun s -> s |> FSharp.transform_enum |> toBuffer |> file.Write)
            structs_seq |> Seq.iter (fun s -> s |> FSharp.transform_struct |> toBuffer |> file.Write)
            fns_seq |> Seq.iter (fun s -> s |> FSharp.transform_function |> toBuffer |> file.Write)                
        | "-cs" -> do
            CSharp.create_template(libname) |> toBuffer |> file.Write
            enums_seq |> Seq.iter (fun s -> s |> CSharp.transform_enum |> toBuffer |> file.Write)
            structs_seq |> Seq.iter (fun s -> s |> CSharp.transform_struct |> toBuffer |> file.Write)
            fns_seq |> Seq.iter (fun s -> s |> CSharp.transform_function |> toBuffer |> file.Write)     
            "}" |> toBuffer |> file.Write           
        | _ -> raise (invalidArg "-generator" $"invalid type {generator}")
        
        file.Flush()
             
    else stdout "invalid arguments\n[filepath] [libname] [type: -fs|-cs] [destination]"



let print_file () =
    let args = Environment.GetCommandLineArgs()
    let src = args[1]
    let generator = args[3]
    let filename = 
        match generator with
        | "-fs" -> src[0..^4] + ".fs"
        | "-cs" -> src[0..^4] + ".cs"
        | _ -> raise (invalidArg "-generator" $"invalid type {generator}")    
    let read = File.ReadAllText(filename)
    stdout read
    

parse_file()

