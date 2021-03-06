﻿
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

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF|ADD|SUB|MUL|DIV|LPAREN|RPAREN

let eat (T: TERMINAL) = function
    | [] -> failwith "Incompleted syntax, program ended early"
    | t::ts when t = T -> ts
    | t::_ -> failwithf "Expected %A, but %A was found instead" T t

let P2_parse program =

    let rec S = function 
        | [] -> failwith "Incompleted syntax, program ended early"
        | IF::ts -> ts |> E |> eat THEN |> S |> eat ELSE |> S
        | BEGIN::ts -> ts |> S |> L 
        | PRINT::ts -> ts |> E
        | t::_ -> failwithf "Expected IF, BEGIN or PRINT. Found %A instead" t

    and L = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | END::ts -> ts 
        | SEMICOLON::ts -> ts |> S |> L
        | t::_ ->  failwithf "Expected END or SEMICOLON. Found %A instead" t

    and E = function        
        | [] -> failwith "Incompleted syntax, program ended early"
        | ID::ts -> ts 
        | t::_ -> failwithf "Expected ID. Found %A instead" t
        
    match (program |> S) with
        | [] -> failwith "Missing EOF"
        | EOF::[] -> printfn "Program Accepted"
        | EOF::_ -> failwith "EOF not at the end of the program"
        | t::_ -> failwithf "Expected EOF, found %A instead" t
 

// P3 - Implement a parser using...

let P3_parse program =

    let rec E tokens = 
        match (T tokens) with
            | [] -> failwith "Incompleted syntax, program ended early"
            | ADD::ts -> ts |> E
            | SUB::ts -> ts |> E
            | t -> t

    and T tokens =
        match (F tokens) with
            | [] -> failwith "Incompleted syntax, program ended early"
            | MUL::ts -> ts |> T
            | DIV::ts -> ts |> T
            | t -> t

    and F tokens =
        match tokens with
            | [] -> failwith "Incompleted syntax, program ended early"
            | ID::ts -> ts
            | LPAREN::ts -> ts |> E |> eat(RPAREN)
            | t::_ -> failwithf "Expected ID or LPAREN. Found %A instead" t
    
    match (program |> E) with
        | [] -> failwith "Missing EOF"
        | EOF::[] -> printfn "Program accepted"
        | EOF::_ -> failwith "EOF not at the end of the program"
        | t::_ -> failwithf "Expected EOF, found %A instead" t
    

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

let optionEvaluate = function
    | None -> "Invalid input"
    | Some n -> sprintf "%A" n


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
    
// P11 - Student record

type Student = {Name: string; Credits: int; GPA: float}

let S1 = {Name = "Jones"; Credits = 109; GPA = 3.85}
let S2 = {Name = "George"; Credits = 60; GPA = 3.15}
let S3 = {Name = "Steve"; Credits = 121; GPA = 2.25}

// P12 - Binary Search Tree remove

type 'a Tree =
    | Branch of 'a * 'a Tree * 'a Tree
    | Leaf

let rec minimum = function
    | Branch (n, Leaf, _) -> n
    | Branch (_, t1, _) -> minimum t1
    | Leaf -> failwith "Tree has no elements"

let rec remove e = function
    | Leaf -> Leaf                              // If element is not found, ignore (return leaf)
    | Branch(n, t1, Leaf) when e = n -> t1        // If element to remove has no right, replace with left child
    | Branch(n,t1,t2) -> 
        if e < n then Branch(n, remove e t1, t2)     // If element is smaller than current, search to remove in the left tree
        elif e > n then Branch(n, t1, remove e t2)   // If element is greater than current, search to remove in the right tree
        else                                     // If element is equal to current, find minimum of the right tree, put it as        
            let m = minimum t2                   // the value on this current branch, and remove the minimum
            Branch(m, t1, remove m t2)
          
// P13 - Parse trees
// Part one:

type pTree = 
    | Lf of TERMINAL
    | Br6 of pTree * pTree * pTree * pTree * pTree * pTree
    | Br3 of pTree * pTree * pTree
    | Br2 of pTree * pTree
 
let P2_parse_with_tree program =

    let rec S = function 
        | [] -> failwith "Incompleted syntax, program ended early"
        | IF::ts -> 
            let (ts, tree_E ) = ts |> E 
            let (ts, tree_S1) = ts |> eat THEN |> S
            let (ts, tree_S2) = ts |> eat ELSE |> S
            (ts, Br6(Lf IF, tree_E, Lf THEN, tree_S1, Lf ELSE, tree_S2))
        | BEGIN::ts -> 
            let (ts, tree_S) = ts |> S 
            let (ts, tree_L) = ts |> L 
            (ts, Br3(Lf BEGIN, tree_S, tree_L))
        | PRINT::ts -> 
            let (ts, tree_E) = ts |> E
            (ts, Br2(Lf PRINT, tree_E))
        | t::_ -> 
            failwithf "Expected IF, BEGIN or PRINT. Found %A instead" t

    and L = function
        | [] -> failwith "Incompleted syntax, program ended early"
        | END::ts -> 
            (ts, Lf END) 
        | SEMICOLON::ts -> 
            let (ts, tree_S) = ts |> S 
            let (ts, tree_L) = ts |> L
            (ts, Br3(Lf SEMICOLON, tree_S, tree_L))
        | t::_ ->  
            failwithf "Expected END or SEMICOLON. Found %A instead" t

    and E = function        
        | [] -> failwith "Incompleted syntax, program ended early"
        | ID::ts -> 
            (ts, Lf ID) 
        | t::_ -> 
            failwithf "Expected ID. Found %A instead" t
        
    match (program |> S) with
        | ([], _) -> failwith "Missing EOF"
        | (EOF::[], p) -> printfn "%A" p
        | (EOF::_, _) -> failwith "EOF not at the end of the program"
        | (t::_, _) -> failwithf "Expected EOF, found %A instead" t

// Part two:

let P3_parse_with_tree program =

    let rec E tokens = 
        match (T tokens) with
            | ([], _) -> failwith "Incompleted syntax, program ended early"
            | (ADD::ts, tree_T) -> 
                let (ts, tree_E) = ts |> E
                (ts, Br3(tree_T, Lf ADD, tree_E))
            | (SUB::ts, tree_T) ->
                let (ts, tree_E) = ts |> E
                (ts, Br3(tree_T, Lf SUB, tree_E))
            | t -> t

    and T tokens =
        match (F tokens) with
            | ([], _) -> failwith "Incompleted syntax, program ended early"
            | (MUL::ts, tree_F) -> 
                let (ts, tree_T) = ts |> T
                (ts, Br3(tree_F, Lf MUL, tree_T))
            | (DIV::ts, tree_F) -> 
                let (ts, tree_T) = ts |> T
                (ts, Br3(tree_F, Lf DIV, tree_T))
            | t -> t 

    and F tokens =
        match tokens with
            | [] -> failwith "Incompleted syntax, program ended early"
            | ID::ts -> 
                (ts, Lf ID)
            | LPAREN::ts -> 
                let (ts, tree_E) = ts |> E 
                let ts = ts |> eat(RPAREN)
                (ts, Br3(Lf LPAREN, tree_E, Lf RPAREN))
            | t::_ -> 
                failwithf "Expected ID or LPAREN. Found %A instead" t
    
    match (program |> E) with
        | ([], _) -> failwith "Missing EOF"
        | (EOF::[], p) -> printfn "%A" p
        | (EOF::_, _) -> failwith "EOF not at the end of the program"
        | (t::_, _) -> failwithf "Expected EOF, found %A instead" t

    

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
    try P2_parse Program_A with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program B -> " 
    try P2_parse Program_B with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program C -> " 
    try P2_parse Program_C with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printfn "\n"
    
//  -------------------

    printfn "Problem 3\n"
    
    let A = [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
    let B = [ID;SUB;ID;MUL;ID;EOF]
    let C = [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF] 
    let D = [ID;MUL;LPAREN;ID;ADD;ID;ADD;RPAREN;EOF]

    printfn "Program A: %A" A
    printfn "Program B: %A" B
    printfn "Program C: %A" C
    printfn "Program D: %A\n" D
    
    printf "Program A -> " 
    try P3_parse A with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program B -> " 
    try P3_parse B with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program C -> " 
    try P3_parse C with | Failure(e) -> printfn "Syntax error: %s" e 
        
    printf "Program D -> " 
    try P3_parse D with | Failure(e) -> printfn "Syntax error: %s" e 
    
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

    printfn "Problem 9\n"
    
    let A = []
    let B = ["cat"]
    let C = [1..5]

    printfn "The last element of %A is %A" A (lastElement A |> optionEvaluate)
    printfn "The last element of %A is %A" B (lastElement B |> optionEvaluate)
    printfn "The last element of %A is %A" C (lastElement C |> optionEvaluate)

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

//  -------------------

    printfn "Problem 11\n"
    
    printfn "Student 1 -> %A" S1
    printfn "Student 2 -> %A" S2
    printfn "Student 3 -> %A" S3
    
    printfn "\n"

//  -------------------

    printfn "Problem 12\n"
    
    let A = Branch(4, Branch(2,Leaf,Leaf), Branch(6,Leaf,Leaf))

    printfn "A = %A \n" A
    printfn "remove 6 A = %A" <| remove 6 A
    printfn "remove 4 A = %A" <| remove 4 A
    printfn "A |> remove 4 |> remove 6 = %A" <| (A |> remove 4 |> remove 6)
    
    printfn "\n"

//  -------------------
 
    printfn "Problem 13 - Part one \n"

    let A = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    let B = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let C = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]

    printfn "Program A: %A" A
    printfn "Program B: %A" B
    printfn "Program C: %A\n" C

    
    printf "Program A -> " 
    try P2_parse_with_tree A with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program B -> " 
    try P2_parse_with_tree B with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program C -> " 
    try P2_parse_with_tree C with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printfn "\n"

   
    printfn "Problem 13 - Part two \n"
    
    let A = [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
    let B = [ID;SUB;ID;MUL;ID;EOF]
    let C = [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF] 
    let D = [ID;MUL;LPAREN;ID;ADD;ID;ADD;RPAREN;EOF]

    printfn "Program A: %A" A
    printfn "Program B: %A" B
    printfn "Program C: %A" C
    printfn "Program D: %A\n" D
    
    printf "Program A -> " 
    try P3_parse_with_tree A with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program B -> " 
    try P3_parse_with_tree B with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printf "Program C -> " 
    try P3_parse_with_tree C with | Failure(e) -> printfn "Syntax error: %s" e 
        
    printf "Program D -> " 
    try P3_parse_with_tree D with | Failure(e) -> printfn "Syntax error: %s" e 
    
    printfn "\n"
    
//  -------------------

    Console.ReadKey() |> ignore
    0
    
    