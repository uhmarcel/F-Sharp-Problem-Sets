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

let rec myListAppend xs ys = 
    match xs, ys with
        | [], [] -> []
        | [], y::ys -> y :: myListAppend [] ys
        | x::xs, ys -> x :: myListAppend xs ys

let rec myListMap2 f xs ys = 
    match xs, ys with
        | [], _ | _, [] -> []
        | x::xs, y::ys -> f x y :: myListMap2 f xs ys

let rec myListReduce f = function
    | [] -> failwith "Can't apply function to empty list"
    | [x] -> x 
    | x::xs -> f x (myListReduce f xs) 

[<EntryPoint>]
let main argv =    
    let A = [1..5]
    let B = [6..10]

    printfn "Rewriting List Library - Testing"
    printfn "List A: %A" A
    printfn "List B: %A\n" B

    let f x = x * x 
    printfn " List.map f A = %A" <| List.map f A
    printfn "myListMap f A = %A\n" <| myListMap f A
    
    printfn " List.sum A = %A" <| List.sum A
    printfn "myListSum A = %A\n" <| myListSum A

    printfn " List.append A B = %A" <| List.append A B 
    printfn "myListAppend A B = %A\n" <| myListAppend A B

    let f x y = x * y

    printfn " List.map2 f A B = %A" <| List.map2 f A B
    printfn "myListMap2 f A B = %A\n" <| myListMap2 f A B

    printfn " List.reduce f A = %A" <| List.reduce f A
    printfn "myListReduce f A = %A" <| myListReduce f A


    Console.ReadKey() |> ignore
    0
