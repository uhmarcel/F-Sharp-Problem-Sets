module Interp

open Parser.Parse
open System


// PCF interpreter

let rec interp = function
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)



// Interpreter abbreviations

let interpfile filename = filename |> parsefile |> interp
let interpstr sourcecode = sourcecode |> parsestr |> interp


// Test Workspace
[<EntryPoint>]
let main argv =
    printfn "Test" 
    Console.ReadKey() |> ignore
    0

