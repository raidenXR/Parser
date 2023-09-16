namespace Parser

[<AutoOpen>]
module Core =
    open System
    open System.Text
    open System.IO
    open System.Linq

    exception InvalidSrcGen of string

    let mutable typesLenghts = 
        [
            "c_int", 4;
            "c_uint", 4;
            "isize", 8;
            "usize", 8;
            "f32", 4;
            "u32", 4;
            "i32", 4;
            "f64", 8;
            "u64", 8;
            "i64", 8;
            "u8", 1;
            "bool", 1;
        ] |> Map.ofList

    let stdout (str: string) = 
        Console.WriteLine(str)

    let suffix (str: string) = 
        let idx0 = str.LastIndexOf('\\')
        let idx1 = str.LastIndexOf('/')
        let idx = Math.Max(idx0, idx1) + 1
        str[(if idx > 0 then idx else 0)..]


    let content (str: string) (a: string) (b: string) =
        let a = if a = null || str.IndexOf(a) < 0 then 0 else str.IndexOf(a) + a.Length
        let b = if b = null || str.IndexOf(b) < 0 then str.Length - 1 else str.IndexOf(b) - 1
        str[a..b].Trim()

    let hasletter (str: string) =
        let rec contains (str: string) (n : int) =
            n < str.Length && ((Char.IsLetter str[n]) || contains(str) (n + 1 ))            
        contains str 0

    let upper (str: string) =
        let mutable arr = str.ToCharArray()
        let idx = arr |> Array.findIndex (fun x -> Char.IsLetter x)
        arr[idx] <- Char.ToUpper arr[idx]
        // arr |> fun cs -> new string(cs)
        new string(arr)

    let readblock (reader: StreamReader) =
        seq {
            while (reader.Peek() <> int(';')) && (not reader.EndOfStream) do
                yield reader.Read() |> char
        } |> Array.ofSeq |> fun (cs) -> new string(cs)

    let enums filename =
        seq {
            use reader = File.OpenText filename
            while not reader.EndOfStream do
                let line = reader.ReadLine()
                if line.Contains "enum(c_int)" then
                    let block = readblock reader 
                    yield line + "\n" + block
        }         

    let structs filename =
        seq {
            use reader = File.OpenText filename
            while not reader.EndOfStream do
                let line = reader.ReadLine()
                if line.Contains "extern struct" then
                    let block = readblock reader 
                    yield line + "\n" + block
        }

    let functions filename =
        seq {
            use textReader = File.OpenText filename
            while not textReader.EndOfStream do
                let line = textReader.ReadLine()            
                if line.Contains "export fn" then
                    let idx = line.IndexOf('{') - 1
                    yield line[0..idx]
        }

