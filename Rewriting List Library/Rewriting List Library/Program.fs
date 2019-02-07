﻿// Rewriting List Library
// The purpose of this project is to rewrite List functions to learn more
// about recursion and F#.

open System

let rec myListMap f = function
    | [] -> []
    | x::xs -> f x :: (myListMap f xs)

let inline myListSum xs = 
    let rec aux = function
        | [] -> LanguagePrimitives.GenericZero
        | x::xs -> x + aux xs
    aux xs

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

let inline myListLength xs = 
    let rec aux = function
        | [] -> LanguagePrimitives.GenericZero
        | _::xs -> LanguagePrimitives.GenericOne + aux xs
    aux xs

let inline myListAverage xs =
    myListSum xs / myListLength xs

let rec myListCollect f = function
    | [] -> []
    | x::xs -> f x @ myListCollect f xs

[<EntryPoint>]
let main argv =    
    let A = [1..5]
    let B = [6..10]
    let C = [1.1; 3.4; 6.2; 2.1]

    printfn "Rewriting List Library - Testing"
    printfn "List A: %A" A
    printfn "List B: %A" B
    printfn "List C: %A\n" C

    let f x = x * x 
    let g x y = x * y
    let h x = [x;x]
    
    // Map
    printfn " List.map f A = %A" <| List.map f A
    printfn "myListMap f A = %A\n" <| myListMap f A
    
    // Sum
    printfn " List.sum A = %A" <| List.sum C
    printfn "myListSum A = %A\n" <| myListSum C

    // Length
    printfn " List.length A = %A" <| List.length A
    printfn "myListLength A = %A\n" <| myListLength A

    // Average
    printfn " List.average C = %A" <| List.average C
    printfn "myListaverage C = %A\n" <| myListAverage C
    // Note: myAverage works only for ints, while list.average works on floats. 

    // Append
    printfn " List.append A B = %A" <| List.append A B 
    printfn "myListAppend A B = %A\n" <| myListAppend A B

    // Map2
    printfn " List.map2 f A B = %A" <| List.map2 g A B
    printfn "myListMap2 f A B = %A\n" <| myListMap2 g A B

    // Reduce
    printfn " List.reduce f A = %A" <| List.reduce g A
    printfn "myListReduce f A = %A\n" <| myListReduce g A

    // Collect
    printfn " List.collect f A = %A" <| List.collect h A
    printfn "myListCollect f A = %A\n" <| myListCollect h A


    Console.ReadKey() |> ignore
    0