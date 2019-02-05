// Rewriting List Library
// The purpose of this project is to rewrite List functions to learn more
// about recursion and F#.

open System

let rec myListMap f = function
    | [] -> []
    | x::xs -> f x :: (myListMap f xs)

let rec myListSum = function
    | [] -> 0
    | x::xs -> x + myListSum xs

[<EntryPoint>]
let main argv =    
    let A = [1..5]
    let B = [7..9]

    printfn "Rewriting List Library - Testing"
    printfn "List A: %A" A
    printfn "List B: %A\n" B

    printfn " List.map f A = %A" <| List.map (fun x -> x * x) A
    printfn "myListMap f A = %A\n" <| myListMap (fun x -> x * x) A
    
    printfn " List.sum A = %A" <| List.sum A
    printfn "myListSum A = %A\n" <| myListSum A
    
    Console.ReadKey() |> ignore
    0
