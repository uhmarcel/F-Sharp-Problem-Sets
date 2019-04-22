module Main

open Parser.Parse
open Interpreter
open Type
open System


[<EntryPoint>]
let main argv =

    printfn "1 - Testing PCF Interpreter \n"
 
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
    displayInterpfile "divisor.pcf"     // Commented out for now to run main faster 
    displayInterpfile "lists.pcf"
    displayInterpfile "ackermann.pcf"
    printfn "\n"
    
    printfn "2 - Testing PCF Type Inference \n"

    infer (NUM 12)
    infer (BOOL true)
    infer (IF(BOOL true, NUM 1, NUM 2))
    infer (IF(BOOL true, IF(BOOL true, NUM 1, NUM 2), IF(BOOL false, NUM 3, NUM 4)))
    infer (SUCC)
    infer (PRED)
    infer (ISZERO)
    infer (APP(SUCC,NUM 5))
    infer (APP(PRED,NUM 5))
    infer (APP(ISZERO,NUM 5))
    infer (IF(APP (ISZERO, NUM 0), APP (SUCC, NUM 5), APP (PRED, NUM 50)))
    infer (IF(APP (ISZERO, NUM 0), APP (ISZERO, NUM 5), APP (ISZERO, NUM 50)))
    printfn ""

    Console.ReadKey() |> ignore
    0

