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
    | IF (e1, e2, e3) ->
        match (interp e1, interp e2, interp e3) with
            | (ERROR s, _, _) -> ERROR s
            | (_, ERROR s, _) -> ERROR s
            | (_, _, ERROR s) -> ERROR s
            | (BOOL true, v1, _)  -> v1
            | (BOOL false, _, v2) -> v2
            | (x, _, _)       -> ERROR (sprintf "'if' expects a bool argument, not %A" x)
    | NUM n -> NUM n
    | BOOL b -> BOOL b
    | SUCC -> SUCC
    | PRED -> PRED
    | ISZERO -> ISZERO
  



// Interpreter abbreviations

let interpfile filename = filename |> parsefile |> interp
let interpstr sourcecode = sourcecode |> parsestr |> interp


// Test Workspace


[<EntryPoint>]
let main argv =

    printfn "Testing PCF Interpreter \n"
    
    let displayInterpstr s = printfn "%A -> %A" s (interpstr s)
    let displayInterpfile s = printfn "File %s -> %A" s (interpfile s)

    // Part A:
    displayInterpstr "succ 0"
    displayInterpstr "succ 1"
    displayInterpstr "pred 0"
    displayInterpstr "pred 10"
    displayInterpstr "succ (succ (succ 0))"
    displayInterpstr "iszero succ"
    displayInterpstr "succ pred 7"
    displayInterpstr "succ (pred 7)"
    displayInterpfile "if.pcf"
    displayInterpfile "complex1.pcf"
    displayInterpfile "complex2.pcf"
    displayInterpfile "complex3.pcf"
    displayInterpfile "complex4.pcf"

    // Parser output test

    printfn "\nTest: %A" <| parsefile "if.pcf"
    printfn "\nTest: %A" <| parsestr "succ 0"

    Console.ReadKey() |> ignore
    0

