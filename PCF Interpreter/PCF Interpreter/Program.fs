module Interp

open Parser.Parse
open System


// PCF interpreter

let rec subst e x t =
    match e with
        | ID n when n = x -> t
        | APP (e1, e2) -> APP (subst e1 x t, subst e2 x t)
        | IF (e1, e2, e3) -> IF (subst e1 x t, subst e2 x t, subst e3 x t)
        | FUN (s, e1) when s <> x -> FUN (s, subst e1 x t)
        | REC (s, e1) when s <> x -> REC (s, subst e1 x t)
        | term -> term

let rec interp = function
    | APP (e1, e2) ->
        match (interp e1, interp e2) with
            | (ERROR s, _) -> ERROR s 
            | (_, ERROR s) -> ERROR s
            | (SUCC, NUM n) -> NUM (n + 1) 
            | (SUCC, x)     -> ERROR <| sprintf "'succ' expects int argument, not '%A'" x
            | (PRED, NUM 0) -> NUM 0
            | (PRED, NUM n) -> NUM (n - 1)
            | (PRED, x)     -> ERROR <| sprintf "'pred' expects int argument, not '%A'" x
            | (ISZERO, NUM 0) -> BOOL true
            | (ISZERO, NUM _) -> BOOL false
            | (ISZERO, x)     -> ERROR <| sprintf "'iszero' expects int argument, not '%A'" x
            | (FUN (s, e1), t) -> interp (subst e1 s t)
            | (_, _) -> ERROR "Invalid application, or not implemented yet"
    | IF (e1, e2, e3) ->
        match (interp e1, e2, e3) with
            | (ERROR s, _, _) -> ERROR s
            | (BOOL true, v1, _)  -> interp v1
            | (BOOL false, _, v2) -> interp v2
            | (x, _, _)       -> ERROR <| sprintf "'if' expects a bool argument, not %A" x
    | REC (s, e1) -> interp (subst e1 s (REC (s, e1)))
    | ID s -> ERROR <| sprintf "Undefined identifier %A" s
    | FUN (s, e1) -> FUN (s, e1)
    | NUM n -> NUM n
    | BOOL b -> BOOL b
    | SUCC -> SUCC
    | PRED -> PRED
    | ISZERO -> ISZERO
    | t -> ERROR <| sprintf "Unrecognized token %A, or not implemented yet" t 
  

// Interpreter abbreviations
let interpfile filename = filename |> parsefile |> interp
let interpstr sourcecode = sourcecode |> parsestr |> interp



[<EntryPoint>]
let main argv =

    printfn "Testing PCF Interpreter \n"
    
    // Testing abbreviations
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
    printfn ""

    // Part C
    displayInterpstr "(fun x -> succ x) 4"
    displayInterpstr "(fun x -> fun y -> fun z -> if iszero x then succ y else pred z) 0 10 20"
    displayInterpstr "(fun x -> fun y -> fun x -> if iszero x then succ y else pred x) 0 10 20"
    displayInterpfile "twice.pcf"
    printfn ""

    // Part D
    displayInterpfile "double.pcf"
    displayInterpfile "minus.pcf"
    displayInterpfile "fibonacci.pcf"
    displayInterpfile "factorial.pcf"
    displayInterpfile "divisor.pcf"
    displayInterpfile "lists.pcf"
    displayInterpfile "ackermann.pcf"


    Console.ReadKey() |> ignore
    0

