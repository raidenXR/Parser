
open System
open System.Text
open Parser
open Core
open FSharp

let printArray (lines: string array) = 
    for i, l in Array.indexed lines do
        printfn "line: %i, %s" i l
    printfn ""

let sb = new StringBuilder(1024)

[<Literal>]
let str = 
    """
    
    /// some comment on enum
    const LL = enum(c_int) {
        lv1,
        lv2,
        lv3,
        lv4,
    };
    
    const ZZ = extern struct {
        f0: f32,    
        f1: f64,    
        f2: u32,    
        f3: u64,    
    };    

    /// some comment on function
    export fn someFunction(v0: f32, v1: [*]const u8, v2: [*c]const [*c]const u8, v3: c_int) void {
        
    }

      
    /// some comment on another function
    export fn someAnotherFunction() void {
        
    }
    
    """

let lines = splitLines str

let entry0 = getEnums lines |> Seq.item 0
printfn "enum"
printArray entry0

let entry1 = getStructs lines |> Seq.item 0
printfn "struct"
printArray entry1

let entry2 = getFunctions lines |> Seq.item 1
printfn "function"
printArray entry2

let fnargs = getFnArgs entry2
let fnname = fnDecl entry2
let fndefinition = fnDefinition entry2
let type' = content ")" "{" fndefinition

printfn "definition: %s" fndefinition
printfn "name: %s" fnname
printfn "type: %s" type'
printArray fnargs

// let comments, body = splitCommentsFromBody entry0
// printfn "comments"
// printArray comments

// printfn "body"
// printArray body

// let decl = typeDecl entry0
// printfn "decl: %s" decl

// let fields' = fields body
// printArray fields'

// printArray comments
let _enum = transformEnum entry0 sb
printfn "%s" _enum

let _struct = transformStruct entry1 sb
printfn "%s" _struct

let _fn = transformFunction entry2 sb
printfn "%s" _fn

// let idx0 = getComments lines 1
// let idx1 = getBlock lines 1

// printfn "idx: %i, %s" idx0 lines[idx0]
// printfn "idx: %i, %s" idx1 lines[idx1] 


[<Literal>]
let src = "mkxk.zig"

let src_lines = readLines src

let enums = getEnums src_lines
let structs = getStructs src_lines
let functions = getFunctions src_lines

// enums
// |> Seq.map (fun e -> transformEnum e sb)
// |> Seq.iter (fun e -> printfn "%s" e)

// structs 
// |> Seq.map (fun s -> transformStruct s sb)
// |> Seq.iter (fun s -> printfn "%s" s)

functions
|> Seq.map (fun f -> transformFunction f sb)
|> Seq.iter (fun f -> printfn "%s" f)

let doc = documentation src
