module Interp

open Parser.Parse
open System
open Parser.Lex


// PCF interpreter

let rec subst e x t =
    match e with
        | ID n when n = x -> t
        | APP (e1, e2) -> APP (subst e1 x t, subst e2 x t)
        | IF (e1, e2, e3) -> IF (subst e1 x t, subst e2 x t, subst e3 x t)
        | FUN (s, e1) when s <> x -> FUN (s, subst e1 x t)
        | REC (s, e1) when s <> x -> FUN (s, subst e1 x t)
        | term -> term

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
            | (_, _) -> ERROR "Not implemented yet"
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
    | _ -> ERROR "Not implemented yet"
  



// Interpreter abbreviations

let interpfile filename = filename |> parsefile |> interp
let interpstr sourcecode = sourcecode |> parsestr |> interp


// Test Workspace


[<EntryPoint>]
let main argv =

    printfn "Testing PCF Interpreter \n"
    
    let displayInterpstr s = printfn "String %A -> %A" s (interpstr s)
    let displayInterpfile s = printfn "File %s -> %A" s (interpfile s)
    let displaySubst e x t = printfn "Subst [%A] [%A -> %A] = %A" e x t (subst e x t)

    // Part A:
    displayInterpstr "succ 0"
    displayInterpstr "succ 1"
    displayInterpstr "pred 0"
    displayInterpstr "pred 10"
    displayInterpstr "succ (succ (succ 0))"
    displayInterpstr "iszero succ"
    displayInterpstr "succ pred 7"
    displayInterpstr "succ (pred 7)"
    displayInterpstr "if succ 4 then 0 else 1"
    displayInterpfile "if.pcf"
    displayInterpfile "complex1.pcf"
    displayInterpfile "complex2.pcf"
    displayInterpfile "complex3.pcf"
    displayInterpfile "complex4.pcf"
    printfn ""

    // Part B
    displaySubst (NUM 6) "a" (NUM 3)
    displaySubst (BOOL true) "a" (NUM 3)
    displaySubst SUCC "a" (NUM 3)
    displaySubst (APP(SUCC, ID "a")) "a" (NUM 3)
    displaySubst (IF (BOOL true, FUN ("a", APP (SUCC, ID "a")), FUN ("b", APP (SUCC, ID "a")))) "a" (NUM 3)

    // Parser output test

    printfn "\n Extra tests"
    printfn "Test: %A" <| parsefile "if.pcf"
    printfn "Test: %A" <| parsestr "succ 0"
    printfn "%A" <| subst (APP(SUCC, ID "x")) "x" (NUM 1)

    Console.ReadKey() |> ignore
    0

