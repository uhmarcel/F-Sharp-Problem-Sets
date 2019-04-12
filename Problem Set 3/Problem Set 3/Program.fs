
open System


// P1 - Building a simple tree.

// Create a discriminated union that can represent a linked list of integers.
// Write a function that converts a list into a linked list of nodes.

type 'a LinkedList =
    | Node of 'a * 'a LinkedList
    | Null

let rec toLinkedList = function
    | [] -> Null
    | x::xs -> Node (x, toLinkedList xs)
    

[<EntryPoint>]
let main argv =

    printfn "Problem 1\n"
    
    printfn "toLinkedList [] = %A" <| toLinkedList []
    printfn "toLinkedList ['do'; 're'] = %A" <| toLinkedList ["do"; "re"]
    printfn "toLinkedList [1..6] = %A" <| toLinkedList [1..6]
    printfn "\n"

//  -------------------

    Console.ReadKey() |> ignore
    0
