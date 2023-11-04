namespace Parser

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


    let readLines (filepath: string) = File.ReadAllLines(filepath)

    let splitLines (text: string) = text.Split("\n")

    let splitArgs (text: string) = text.Split(",")

    let concat (lines: string array) = String.concat "\n" lines
    
    let hasletter (str: string) =
        let rec contains (str: string) (n : int) =
            n < str.Length && ((Char.IsLetter str[n]) || contains(str) (n + 1 ))            
        contains str 0

    /// check if comment lines are not empty
    let rec hasContent (lines: string array) (n: int) =
        if n = lines.Length then false
        elif (hasletter lines[n]) then true 
        else hasContent lines (n + 1)
        
    let upper (str: string) =
        let mutable arr = str.ToCharArray()
        let idx = arr |> Array.findIndex (fun x -> Char.IsLetter x)
        arr[idx] <- Char.ToUpper arr[idx]
        // arr |> fun cs -> new string(cs)
        new string(arr)

    /// advances until it finds a line that contains str
    let rec advance (lines: string array) (str: string) (n: int) =
        if lines[n].Contains(str) || n > lines.Length then n else advance lines str (n + 1)

    /// advances as long as the str exists in line
    let rec advance2 (lines: string array) (str: string) (n: int) =
        if not (lines[n].Contains(str)) || n > lines.Length then n else advance lines str (n + 1)

    /// retreats until it finds a line that contains str
    let rec prev (lines: string array) (str: string) (n: int) =
        if lines[n].Contains(str) || n < 1 then n else prev lines str (n - 1)

    /// retreats as long as the str exists in line
    let rec prev2 (lines: string array) (str: string) (n: int) =
        if not (lines[n].Contains(str)) || n < 1 then n else prev lines str (n - 1)

    let getComments (lines: string array) (n: int) = prev2 lines @"\\\" (n - 1)

    let getBlock (lines: string array) (n: int) = advance lines "}" (n + 1)

    let split (str: string) = 
        let arr = str.Split(':')
        (arr[0].Trim(), arr[1].Trim())
    
    let append (str: string) (sb: StringBuilder) = sb.Append(str)
    
    let appendln (str: string) (sb: StringBuilder) = sb.AppendLine(str)
    
    let clear (sb: StringBuilder) = sb.Clear() |> ignore; sb    
    
    let copy (lines: string array) (sb: StringBuilder) =
        for line in lines do
            if hasletter line then
                sb |> appendln $"    {line}" |> ignore
        sb


    let splitCommentsFromBody (lines: string array) = 
        let n_comments = advance2 lines @"\\\" 0
        lines[0..n_comments], lines[(n_comments + 1)..]

        
    let content (a: string) (b: string) (str: string) =
        let a = if a = null || str.IndexOf(a) < 0 then 0 else str.IndexOf(a) + a.Length
        let b = if b = null || str.IndexOf(b) < 0 then str.Length - 1 else str.IndexOf(b) - 1
        str[a..b].Trim()

    // let slice (a: int) (b: int) (items: 'T array) = items[a..^b]   

    let definition (body: string array) = body[0]
    
    let fields (body: string array) = body[1..^1] 

    let typeDecl = splitCommentsFromBody >> snd >> Array.item 0 >> content "const" "="


    let fnDecl = splitCommentsFromBody >> snd >> Array.item 0 >> content "fn" "("

    let fnDefinition = splitCommentsFromBody >> snd >> Array.item 0
    
    let getFnArgs = concat >> content "(" ")"  >> splitArgs 
    
    let getSequence (identifier: string) (lines: string array) = 
        seq {
            let mutable idx = 0
            while idx < lines.Length do
                if lines[idx].Contains(identifier) then
                    let a = getComments lines idx
                    let b = getBlock lines idx
                    yield lines[a..b]
                    idx <- b                
                idx <- idx + 1
        }

    let getEnums (lines: string array) = getSequence "enum(c_int)" lines
    
    let getStructs (lines: string array) = getSequence "extern struct" lines
    
    let getFunctions (lines: string array) = getSequence "export fn" lines
        

    let stdout (str: string) = Console.WriteLine(str)

    let suffix (str: string) = 
        let idx0 = str.LastIndexOf('\\')
        let idx1 = str.LastIndexOf('/')
        let idx = Math.Max(idx0, idx1) + 1
        str[(if idx > 0 then idx else 0)..]




    let readblock (reader: StreamReader) =
        seq {
            while (reader.Peek() <> int(';')) && (not reader.EndOfStream) do
                yield reader.Read() |> char
        } |> Array.ofSeq |> fun (cs) -> new string(cs)

    // let enums filename =
    //     seq {
    //         use reader = File.OpenText filename
    //         while not reader.EndOfStream do
    //             let line = reader.ReadLine()
    //             if line.Contains "enum(c_int)" then
    //                 let block = readblock reader 
    //                 yield line + "\n" + block
    //     }         

    // let structs filename =
    //     seq {
    //         use reader = File.OpenText filename
    //         while not reader.EndOfStream do
    //             let line = reader.ReadLine()
    //             if line.Contains "extern struct" then
    //                 let block = readblock reader 
    //                 yield line + "\n" + block
    //     }

    // let functions filename =
    //     seq {
    //         use textReader = File.OpenText filename
    //         while not textReader.EndOfStream do
    //             let line = textReader.ReadLine()            
    //             if line.Contains "export fn" then
    //                 let idx = line.IndexOf('{') - 1
    //                 yield line[0..idx]
    //     }


    [<Literal>]
    let style = 
        """<style>
          :root {
            font-size: 1em;
            --ui: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji";
            --mono: "Source Code Pro", monospace;
            --tx-color: #141414;
            --bg-color: #ffffff;
            --link-color: #2A6286;
            --sidebar-sh-color: rgba(0, 0, 0, 0.09);
            --sidebar-mod-bg-color: #f1f1f1;
            --sidebar-modlnk-tx-color: #141414;
            --sidebar-modlnk-tx-color-hover: #fff;
            --sidebar-modlnk-tx-color-active: #000;
            --sidebar-modlnk-bg-color: transparent;
            --sidebar-modlnk-bg-color-hover: #555;
            --sidebar-modlnk-bg-color-active: #FFBB4D;
            --search-bg-color: #f3f3f3;
            --search-bg-color-focus: #ffffff;
            --search-sh-color: rgba(0, 0, 0, 0.18);
            --search-other-results-color: rgb(100, 100, 100);
            --modal-sh-color: rgba(0, 0, 0, 0.75);
            --modal-bg-color: #aaa;
            --warning-popover-bg-color: #ff4747;
          }
        body {
          font-size: 1rem;
          font-family: sans-serif;
          margin: 20px;
        }
        ul {
          line-height: 25px;
        }
        code {
          font-family: var(--mono);
          font-weight: bold;
          font-size: 1em;
          <!-- background: whitesmoke;
          color: #000;
          padding: 10px;
          line-height: 10px; -->       
        }
        span {
          margin: 20px;
          padding: 3px;
          line-height: 45px;
        }
        dl {
          font-family: var(--mono);
          <!-- line-height: 45px; -->
        }
        dt {
          margin-bottom: 5px;  
        }
        dd {
          overflow: auto;
          margin-bottom:  20px;  
        }
        </style>
    """

    [<Literal>]
    let doctype = 
        """<!DOCTYPE html>
        <html>
          <head>
        """

    [<Literal>]
    let closedoc =
        """
          </head>
        </html>    
        """

    let context (comment: string) = 
        let idx = comment.LastIndexOf('/')
        if idx > 0 then comment[(idx + 1)..] else comment

    let headerWrite (header: string) (lv: int) =
        match lv with
        | 1 -> appendln $"  <h1>{header}</h1>"
        | 2 -> appendln $"  <h2>{header}</h2>"
        | 3 -> appendln $"  <h3>{header}</h3>"
        | _ -> appendln $"  <h4>{header}</h4>"


    let writeComments (entry: string array) (sb: StringBuilder) =
        let comments, _ = splitCommentsFromBody entry
        for comment in comments do
            sb |> appendln $"    {(context comment)} </br>" |> ignore
        sb


    let typeWrite (entry: string array) =
        appendln $"  <li>{(typeDecl entry)}</li>" >> writeComments entry >> appendln ""

    let functionWrite (entry: string array) =
        append "    <dt><code>" 
        >> append ((fnDefinition entry).TrimEnd('{'))
        >> appendln "</code><dt>" 
        >> append "    <dd>"
        >> writeComments entry
        >> appendln "    </dd>\n"


    let documentation (filepath: string) =
        let lines = readLines filepath
        let enums = getEnums lines
        let structs = getStructs lines
        let functions = getFunctions lines
        
        let sb = new StringBuilder(10 * 1024)
        let name = suffix filepath
        use fs = File.CreateText(name[0..^4] + "_documentation.html") 

        fs.Write(doctype)
        fs.Write(style)

        sb |> headerWrite "Enums" 3 |> appendln "<ul>" |> ignore
        for enum in enums do
            ignore (typeWrite enum sb)
        sb |> appendln "</ul>" |> ignore

        sb |> headerWrite "Structs" 3 |> appendln "<ul>" |> ignore
        for strct in structs do
            ignore (typeWrite strct sb)
        sb |> appendln "</ul>" |> ignore

        sb |> headerWrite "Functions" 3 |> appendln "<dt>" |> ignore
        for fn in functions do
            ignore (functionWrite fn sb)
        sb |> appendln "</dt>" |> ignore
        
        fs.Write(sb.ToString())
        fs.Write(closedoc)
        
        
        
