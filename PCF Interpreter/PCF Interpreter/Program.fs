module Interp

open Parser.Parse
open System


// PCF interpreter

let rec interp = function
    | APP (e1, e2) ->
        match (interp e1, interp e2) with
            | (ERROR s, _) -> ERROR s 
            | (_, ERROR s) -> ERROR s
            | (SUCC, NUM n) -> NUM (n + 1) 
            | (SUCC, x)     -> ERROR (sprintf "'succ' expects int argument, not '%A'" x)
            | (PRED, NUM 0) -> ERROR "'pred' is not defined for zero"
            | (PRED, NUM n) -> NUM (n - 1)
            | (PRED, x)     -> ERROR (sprintf "'pred' expects int argument, not '%A'" x)
            | (ISZERO, NUM 0) -> BOOL true
            | (ISZERO, NUM _) -> BOOL false
            | (ISZERO, x)     -> ERROR (sprintf "'iszero' expects int argument, not '%A'" x)
    | t -> t



// Interpreter abbreviations

let interpfile filename = filename |> parsefile |> interp
let interpstr sourcecode = sourcecode |> parsestr |> interp


// Test Workspace


[<EntryPoint>]
let main argv =

    printfn "Testing PCF Interpreter \n"
    
    let displayInterpstr s = printfn "%s -> %A" s (interpstr s)
    let displayInterpfile s = printfn "%s -> %A" s (interpfile s)

    displayInterpstr "succ 0"
    displayInterpstr "succ 1"
    displayInterpstr "pred 0"
    displayInterpstr "pred 10"
    displayInterpstr "succ (succ (succ 0))"
    displayInterpstr "iszero succ"
    displayInterpstr "succ pred 7"
    displayInterpstr "succ (pred 7)"

    // Parser output test

    printfn "\nTest: %A" <| parsestr "succ (succ (succ 0))"

    Console.ReadKey() |> ignore
    0

