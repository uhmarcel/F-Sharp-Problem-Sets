
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


// P3 - A palindrome is a word that is spelled the same backwards as forwards. Our palindromes
// will have a vertical bar in the middle, to separate the first half from the second half.

// a) Write a CFG to recognize palindromes over the alphabet {a, b, |}, with the bar in the middle.
    
    // S -> aSa | bSb | '|'

// b) Write a parse function that accepts a string and generates tokens for the language.

type P3_TOKEN = A | B | BAR | EOF | UNDEF

let parse_P3 string =
    let rec loop (s: string) tokens = function
        | -1 -> tokens
        | n  -> match s.[n] with
                  | 'a' -> loop s (A::tokens) (n-1)
                  | 'b' -> loop s (B::tokens) (n-1)
                  | '|' -> loop s (BAR::tokens) (n-1)
                  | _ -> loop s (UNDEF::tokens) (n-1)
    loop string [EOF] (string.Length - 1)    



// P5 - Write a tail-recursive F# function interleave(xs,ys) that interleaves two lists
// Compare the timing of the recursive function from Problem Set 1 with this tail-recursive
// version. Time these examples in both versions.

let interleave xs ys =
    let rec loop acc = function
        | [], [] -> List.rev acc
        | [], _ | _, [] -> failwith "Lists are not the same size"
        | x::xs, y::ys -> loop (y::x::acc) (xs, ys)
    loop [] (xs, ys)
        

// P8 - Create a tail-recursive function that has a big integer as input and calculates 2I 
// raised to that power.
// Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.

let exponential n =
    let rec loop acc n = 
        if n = 0I then 1I
        elif n = 1I then acc
        else loop (2I * acc) (n - 1I) 
    loop 2I n


        

[<EntryPoint>]
let main argv =

    printfn "Problem 1\n"
    
    printfn "toLinkedList [] = %A" <| toLinkedList []
    printfn "toLinkedList ['do'; 're'] = %A" <| toLinkedList ["do"; "re"]
    printfn "toLinkedList [1..6] = %A" <| toLinkedList [1..6]

    printfn "\n"

    //  -------------------
    
    printfn "Problem 3\n"
    
    printfn "Part A:"
    printfn "S -> aSa | bSb | '|' \n"
    
    printfn "Part B:"
    printfn "parser 'ab|ba' = %A" <| parse_P3 ""
    printfn "parser 'ab|ba' = %A" <| parse_P3 "ab|ba"
    printfn "parser 'ab|ba' = %A \n" <| parse_P3 "aac|baa"

    printfn "\n"

    //  -------------------

    printfn "Problem 5\n"
    
    printfn "interleave [] [] = %A" <| interleave [] []
    printfn "interleave [3;5;7] [4;6;8] = %A" <| interleave [3;5;7] [4;6;8]
    printfn "interleave [1..6] [3..9] = %A" <| interleave [1..6] [4..9]
    printfn "interleave ['how'; 'you'] ['are'; 'doing'] = %A" <| interleave ["how"; "you"] ["are"; "doing"]
  
    printfn "\n"

    //  -------------------
    
    printfn "Problem 8\n"
    
    printfn "exponential 0I = %A" <| exponential 0I
    printfn "exponential 1I = %A" <| exponential 1I
    printfn "exponential 2I = %A" <| exponential 2I
    printfn "exponential 4I = %A" <| exponential 4I
    printfn "exponential 16I = %A" <| exponential 16I
    printfn "exponential 256I = %A" <| exponential 256I
    //printfn "exponential 1024I = %A" <| exponential 1024I
    //printfn "exponential 65536I = %A" <| exponential 65536I

    printfn "\n"

    //  -------------------
    Console.ReadKey() |> ignore
    0
