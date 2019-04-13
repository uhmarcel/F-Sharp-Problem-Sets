
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


// P5 - Write a tail-recursive F# function interleave(xs,ys) that interleaves two lists
// Compare the timing of the recursive function from Problem Set 1 with this tail-recursive
// version. Time these examples in both versions.

let interleave xs ys =
    let rec loop acc = function
        | [], [] -> List.rev acc
        | [], _ | _, [] -> failwith "Lists are not the same size"
        | x::xs, y::ys -> loop (y::x::acc) (xs, ys)
    loop [] (xs, ys)
        
        

[<EntryPoint>]
let main argv =

    printfn "Problem 1\n"
    
    printfn "toLinkedList [] = %A" <| toLinkedList []
    printfn "toLinkedList ['do'; 're'] = %A" <| toLinkedList ["do"; "re"]
    printfn "toLinkedList [1..6] = %A" <| toLinkedList [1..6]
    printfn "\n"

    //  -------------------

    printfn "Problem 5\n"
    
    printfn "interleave [] [] = %A" <| interleave [] []
    printfn "interleave [3;5;7] [4;6;8] = %A" <| interleave [3;5;7] [4;6;8]
    printfn "interleave [1..6] [3..9] = %A" <| interleave [1..6] [4..9]
    printfn "interleave ['how'; 'you'] ['are'; 'doing'] = %A" <| interleave ["how"; "you"] ["are"; "doing"]
  
    printfn "\n"

    //  -------------------
    Console.ReadKey() |> ignore
    0
