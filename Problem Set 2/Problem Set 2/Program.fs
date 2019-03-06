
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
    
    let eat (T: TERMINAL) = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | t::ts when t = T -> ts
        | t::_ -> failwithf "Expected %A, but %A was found instead" T t

    let rec S = function 
        | [] -> failwith "Incompleted syntax, program ended early"
        | t::ts -> 
            match t with
                | IF -> ts |> eat ID |> eat THEN |> S |> eat ELSE |> S
                | BEGIN -> ts |> S |> L 
                | PRINT -> ts |> eat ID
                | _ -> failwithf "Expected IF, BEGIN or PRINT. Found %A instead" t
    
    and L = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | t::ts ->
            match t with
                | END -> ts 
                | SEMICOLON -> ts |> S |> L
                | _ ->  failwithf "Expected END or SEMICOLON. Found %A instead" t

    let accept = printfn "Program accepted"
    let error =  printfn "Syntax error" 

    let parseProgram p =
        let parsed = p |> S
        match parsed with
            | [] -> failwith "Missing EOF or terminated early"
            | t::_ -> if t = EOF then accept else error
    
    printfn "Testing Syntax parse"
           
     
       
            
    
    



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
    
    