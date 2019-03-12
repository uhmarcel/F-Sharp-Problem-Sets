
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


// P2 - Creating a Syntax Parser

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF|ADD|SUB|MUL|LPAREN|RPAREN

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
 

// P3 - Implement a parser using...

//let ProblemThree =
    
//    let rec E = function
//        | [] -> "Incompleted syntax, program ended early"
//        | 
    
// WIP



// P4 - Define an F# function curry f that converts an uncurried function to a 
// curried function, and an F# function uncurry f that does the opposite conversion. 

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b


// P5 - Given vectors u = (u1, u2,..., un) and v = (v1, v2,..., vn), the inner product
// of u and v is defined to be u1*v1 + u2*v2 + ... + u n*vn. Write a curried F# function
// inner that takes two vectors represented as int list and returns their inner product.
    
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
        
   
    

// P6 - Given an m-by-n matrix A and an n-by-p matrix B, the product of A and B is an
// m-by-p matrix whose entry in position (i,j) is the inner product of row i of A with 
// column j of B.
    
let rec transpose = function
    | []::_ -> []
    | xs -> List.map (fun x -> List.head x) xs :: transpose (List.map (fun x -> List.tail x) xs)

let rec innerProduct xs ys = 
    match xs, ys with 
        | [], [] -> 0
        | _, [] | [], _ -> failwith "Lists must have same size"
        | x::xs, y::ys -> x * y + innerProduct xs ys 
    
let matrixProduct A B =
    let rec product_aux ys = function
        | [] -> []
        | x::xs -> List.map (innerProduct x) ys :: product_aux ys xs    
    let B = transpose B
    product_aux B A


// P9 - Write a recursive function that returns the last element in its list parameter, 
// using the option type to handle invalid input.

let rec lastElement = function
    | [] -> None
    | x :: [] -> Some x
    | _ :: xs -> lastElement xs





// P10 - write an F# program to evaluate arithmetic expressions written in the language 
// given by the following context-free grammar:
// E -> n | -E | E + E | E - E | E * E | E / E | (E)

type Exp =
    | Num of int
    | Neg of Exp
    | Sum of Exp * Exp
    | Diff of Exp * Exp
    | Prod of Exp * Exp
    | Quot of Exp * Exp    
  
let rec evaluate = function
    | Num n -> Some n
    | Neg e -> 
        match evaluate e with
            | Some n -> Some (-n)
            | _ -> None
    | Sum (e1, e2) -> 
        match evaluate e1, evaluate e2 with
            | Some n1, Some n2 -> Some (n1 + n2)
            | _ -> None
    | Diff (e1, e2) -> 
        match evaluate e1, evaluate e2 with
            | Some n1, Some n2 -> Some (n1 - n2)
            | _ -> None
    | Prod (e1, e2) -> 
        match evaluate e1, evaluate e2 with
            | Some n1, Some n2 -> Some (n1 * n2)
            | _ -> None
    | Quot (e1, e2) -> 
        match evaluate e1, evaluate e2 with
            | Some _, Some 0 -> None 
            | Some n1, Some n2 -> Some (n1 / n2)
            | _ -> None
    
    

[<EntryPoint>]
let main argv =
    
    printfn "Problem 1\n"

    printfn "Tuple A = %A" A
    printfn "Threeple B = %A" B
    printfn "Fourple C = %A\n" C
    
    printfn "coordinateReduce (+) A = %A" <|coordinateReduce (+) A
    printfn "coordinateReduce (+) B = %A" <|coordinateReduce (+) B
    printfn "coordinateReduce (+) C = %A" <|coordinateReduce (+) C
    
    printfn "coordinateReduce (-) A = %A" <| coordinateReduce (-) A
    printfn "coordinateReduce (-) B = %A" <| coordinateReduce (-) B
    printfn "\n"

//  -------------------

    printfn "Problem 2\n"
    
    let Program_A = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    let Program_B = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let Program_C = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]

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
    
//  -------------------

    printfn "Problem 4\n"
    printf "%A - " <| (+) 2 4                       // No syntax error, it is curried
    printf "%A - " <| uncurry (+) (2,3)             // No syntax error, it is uncurried
    printf "%A \n" <| curry (fun (x,y) -> x*y) 4 6  // No syntax error, it is curried
    
    printfn "val curry : f:('a * 'b -> 'c) -> a:'a -> b:'b -> 'c"
    printfn "val uncurry : f:('a -> 'b -> 'c) -> a:'a * b:'b -> 'c"

    printfn "\n"

//  -------------------
    
    printfn "Problem 5\n"

    printfn "Inner function result -> Stack overflow" // <| inner [1I..50000I][50001I..100000I] -> Stack overflow!
    printfn "Inner_tail function result -> %A" <| inner_tail [1I..50000I][50001I..100000I]
    
    printfn "\n"

//  -------------------

    printfn "Problem 6\n"
    
    let Matrix_A = [[1;2;3];[4;5;6]]
    let Matrix_B = [[0;1];[3;2];[1;2]]
        
    printfn "Matrix A: %A" Matrix_A
    printfn "Matrix B: %A" Matrix_B
    printfn "Matrix A * Matrix B = %A" <| matrixProduct Matrix_A Matrix_B    

    printfn "\n"

//  -------------------

    printfn "Problem 10\n"
    
    let Expression_A = Prod(Num 3, Diff(Num 5, Num 1))
    let Expression_B = Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0)))
    let Expression_C = Prod(Sum(Num 5, Neg (Num 2)), Quot(Diff(Prod(Num 6, Num 3), Num 5), Num 2))

    printfn "Expression A: %A" Expression_A
    printfn "evaluate(Expression A) = %A" <| evaluate Expression_A
    printfn "Expression B: %A" Expression_B
    printfn "evaluate(Expression B) = %A" <| evaluate Expression_B
    printfn "Expression C: %A" Expression_C
    printfn "evaluate(Expression C) -> %A" <| evaluate Expression_C

    printfn "\nNote: None = %A" None
    printfn "(Expression B returned None as it has a division by 0)"
   
    printfn "\n"

    Console.ReadKey() |> ignore
    0
    
    