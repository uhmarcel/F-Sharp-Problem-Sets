
open System
open System.Runtime.Serialization


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
    printfn "coordinateReduce (+) C = %A" <|coordinateReduce (+) C

    // e) Call the function with (-) for the numeric Coordinates in part (b). 
    printfn "coordinateReduce (-) A = %A" <| coordinateReduce (-) A
    printfn "coordinateReduce (-) B = %A" <| coordinateReduce (-) B
    printfn "\n"
    0


// P2 - Creating a Syntax Parser

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF|ADD|SUB|MUL|LPAREN|RPAREN

let ProblemTwo =

    let eat (T: TERMINAL) = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | t::ts when t = T -> ts
        | t::_ -> failwithf "Expected %A, but %A was found instead" T t

    let rec S = function 
        | [] -> failwith "Incompleted syntax, program ended early"
        | IF::ts -> ts |> eat ID |> eat THEN |> S |> eat ELSE |> S
        | BEGIN::ts -> ts |> S |> L 
        | PRINT::ts -> ts |> eat ID
        | t::_ -> failwithf "Expected IF, BEGIN or PRINT. Found %A instead" t

    
    and L = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | END::ts -> ts 
        | SEMICOLON::ts -> ts |> S |> L
        | t::_ ->  failwithf "Expected END or SEMICOLON. Found %A instead" t

    let parseProgram p =
        let parsed = p |> S       
        match parsed with
            | [] -> failwith "Missing EOF or terminated early"
            | EOF::[] -> printfn "Program Accepted"
            | EOF::_ -> failwith "EOF not at the end of the program"
            | t::_ -> failwithf "Expected EOF, found %A instead" t

    
    let Program_A = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    let Program_B = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let Program_C = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]

    printfn "Problem 2\n"

    printfn "Program A: %A" Program_A
    printfn "Program B: %A" Program_B
    printfn "Program C: %A\n" Program_C

    
    printf "Program A -> " 
    try parseProgram Program_A with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program B -> " 
    try parseProgram Program_B with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program C -> " 
    try parseProgram Program_C with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printfn "\n"
    0
 

// P3 - Implement a parser using...

//let ProblemThree =
    
//    let rec E = function
//        | [] -> "Incompleted syntax, program ended early"
//        | 
    
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

    printfn "\n"
    0


let ProblemFive =
    
    let rec inner us vs = 
        match us, vs with
            | [], [] -> 0I
            | [], _ | _, [] -> failwith "Lists are not the same size"
            | u::us, v::vs -> u*v + inner us vs
     
    let inner_tail us vs =
        let rec inner_aux xs ys acc = 
            match xs, ys with
                | [], [] -> acc
                | [], _ | _, [] -> failwith "Lists are not the same size"
                | x::xs, y::ys -> inner_aux xs ys (acc + x*y)
        inner_aux us vs 0I
        
    printfn "Problem 5\n"

    printfn "Inner function result -> Stack overflow" // <| inner [1I..50000I][50001I..100000I] -> Stack overflow!
    printfn "Inner_tail function result -> %A" <| inner_tail [1I..50000I][50001I..100000I]
    
    printfn "\n"
    0



[<EntryPoint>]
let main argv =
    
    ProblemOne |> ignore
    ProblemTwo |> ignore
    //ProblemThree |> ignore
    ProblemFour |> ignore
    ProblemFive |> ignore
    
    Console.ReadKey() |> ignore
    0
    
    