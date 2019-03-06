
open System


// P1 - Discriminated Unions

// a) Create a discriminated union for Coordinates that can be a Tuple, Threeple
//    or Fourple that represent tuples of size two, three and four. 
//    The type for the union should be polymorphic.

type Coordinates<'a> =
    | Tuple of 'a * 'a
    | Threeple of 'a * 'a * 'a
    | Fourple of 'a * 'a * 'a * 'a

    
// b) Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.

let A = Tuple (2, 6)
let B = Threeple (1.3, 4.2, 6.3)
let C = Fourple ("and", "or", "not", "xor")


// c) Create a function that has a parameter of a binary function and Coordinate. 
//    Apply the function to the Coordinate like List.reduce.

let rec coordinateReduce f = function
    | Tuple (r,s) -> f r s
    | Threeple (r,s,t) -> Tuple (f r s, t) |> coordinateReduce f
    | Fourple (r,s,t,v) -> Threeple (f r s, t, v) |> coordinateReduce f



let ProblemOne =  
    printfn "Problem 1\n"
    printfn "Tuple A = %A" A
    printfn "Threeple B = %A" B
    printfn "Fourple C = %A\n" C
    
    // d) Call the function with (+) for each of the Coordinates in part (b).

    printfn "coordinateReduce (+) A = %A" <|coordinateReduce (+) A
    printfn "coordinateReduce (+) B = %A" <|coordinateReduce (+) B
    printfn "coordinateReduce (+) C = %A\n" <|coordinateReduce (+) C

    // e) Call the function with (-) for the numeric Coordinates in part (b). 

    printfn "coordinateReduce (-) A = %A" <| coordinateReduce (-) A
    printfn "coordinateReduce (-) B = %A" <| coordinateReduce (-) B


// P2 - Creating a Syntax Parser
type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let ProblemTwo =
    let eat expToken = function
        | [] -> failwith "Expecting a token but instead program terminated early"
        | x::xs when x = expToken -> xs
        | x::_ -> failwith <| sprintf "Expected token %A, but %A found instead" expToken x

    let rec S = function
        | [] -> failwith "Expecting a token but instead program terminated early"
        | x::xs -> match x with
            | IF -> xs |> eat ID |> eat THEN |> S |> eat ELSE |> S
            | BEGIN -> xs |> S |> L
            | PRINT -> xs |> eat ID
            | EOF -> x::xs
            | _ -> failwith <| sprintf "Expected IF, BEGIN, PRINT or EOF, but got %A" x
            
    let rec L = function
        | [] -> failwith "Expecting a token but instead program terminated early"
        | x::xs -> match x with
            | END -> xs
            | SEMICOLON -> xs |> S |> L
            | _ -> failwith "Wrong stuff"

     
       
            
    
    



// WIP


// P3 - Implement a parser using...
// WIP


// P4 - Define an F# function curry f that converts an uncurried function to a 
// curried function, and an F# function uncurry f that does the opposite conversion. 

let ProblemFour =
    let curry f a b = f (a,b)
    let uncurry f (a,b) = f a b
    
    printfn "Problem 4\n"
    printf "%A - " <| (+) 2 4                       // No syntax error, it is curried
    printf "%A - " <| uncurry (+) (2,3)             // No syntax error, it is uncurried
    printf "%A \n" <| curry (fun (x,y) -> x*y) 4 6  // No syntax error, it is curried
    
    printfn "val curry : f:('a * 'b -> 'c) -> a:'a -> b:'b -> 'c"
    printfn "val uncurry : f:('a -> 'b -> 'c) -> a:'a * b:'b -> 'c"




[<EntryPoint>]
let main argv =
    ProblemOne
    Console.ReadKey() |> ignore
    0
    
    