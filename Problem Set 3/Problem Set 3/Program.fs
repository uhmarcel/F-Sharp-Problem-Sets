﻿
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

// c) Write a syntax checker that verifies if a list of tokens represents a palindrome.

let eat (T: P3_TOKEN) = function
    | [] -> failwithf "Expected %A" T
    | t::ts when t = T -> ts
    | t::_ -> failwithf "Expected %A, but found %A intead" T t
    
let syntax_P3 program =
    
    let rec S = function 
        | [] -> failwith "Program ended early"
        | A::ts -> ts |> S |> eat A
        | B::ts -> ts |> S |> eat B
        | BAR::ts -> ts
        | t::_ -> failwithf "Expected A, B or BAR. Found %A instead." t
    
    match (program |> S) with
        | [] -> failwith "Missing EOF"
        | EOF::[] -> printfn "Program Accepted"
        | EOF::_ -> failwith "EOF not at the end of the program"
        | t::_ -> failwithf "Expected EOF, found %A instead" t

// d) Extend the syntax checker so it generates an abstract syntax tree and displays it, 
// for valid palindromes.

type SyntaxTree =
    | Br3 of SyntaxTree * SyntaxTree * SyntaxTree  
    | Lf of P3_TOKEN

let syntax_tree_P3 program =
    
    let rec S = function 
        | [] -> failwith "Program ended early"
        | A::ts -> 
            let (ts, tree_S) = ts |> S
            let ts = ts |> eat A
            (ts, Br3(Lf A, tree_S, Lf A))
        | B::ts -> 
            let (ts, tree_S) = ts |> S
            let ts = ts |> eat B
            (ts, Br3(Lf B, tree_S, Lf B))
        | BAR::ts -> 
            (ts, Lf BAR)
        | t::_ -> 
            failwithf "Expected A, B or BAR. Found %A instead." t
    
    match (program |> S) with
        | ([], _) -> failwith "Missing EOF"
        | (EOF::[], p) -> printfn "%A" p
        | (EOF::_, _) -> failwith "EOF not at the end of the program"
        | (t::_, _) -> failwithf "Expected EOF, found %A instead" t


// P5 - Write a tail-recursive F# function interleave(xs,ys) that interleaves two lists
// Compare the timing of the recursive function from Problem Set 1 with this tail-recursive
// version. Time these examples in both versions.

let interleave xs ys =
    let rec loop acc = function
        | [], [] -> List.rev acc
        | [], _ | _, [] -> failwith "Lists are not the same size"
        | x::xs, y::ys -> loop (y::x::acc) (xs, ys)
    loop [] (xs, ys)
        

// P6 - Alternating series
// a) Generate an infinite sequence for the alternating series of 1/(2**n):
// b) Display the 5th through 15th numbers in the series. 
// c) Repeat the exercise using an infinite stream.

let alternatingSeq = Seq.initInfinite( fun n -> (-1.0)**float(n) / (2.0**float(n + 1)) ) 
let alternatingSeqEnum = alternatingSeq.GetEnumerator()

type 'a InfiniteStream =
    Cons of 'a * (unit -> 'a InfiniteStream)

let getNth stream n = 
    let rec inner (Cons (x, xsf)) = function
        | 0 -> failwith "Undefined zero-th element"
        | 1 -> x
        | n -> inner (xsf()) (n-1)
    inner (stream LanguagePrimitives.GenericZero) n

let rec alternatingStream n =  Cons ((-1.0)**n / 2.0**(n + 1.0), fun () -> alternatingStream (n + 1.0))


// P7 - Multiples of a list
// a) Generate an infinite stream for the the natural numbers greater than zero 
// that are divisible by each element in a list of four elements. Use four, nested
// calls of filter on the infinite stream of natural numbers starting at one.
// b) Display the 20th through 30th numbers in the series.
// c) Repeat the exercise using an infinite sequence. Sequences also have a filter function, 
// so it can be solved similarly to the infinite stream version. 
// Just for fun, try to solve it without using the filter function.
// d) For both functions, be sure to dislay an appropriate error message if the list does not 
// have exactly four elements.

let rec filter f (Cons (x, xsf)) = 
    if f x then Cons (x, fun () -> filter f (xsf()))
    else filter f (xsf())

let rec take n (Cons (x, xsf)) =
    if n = 0 then []
    else x :: take (n-1) (xsf())

let rec drop n (Cons (x, xsf)) =
    if n = 0 then (Cons (x, xsf))
    else drop (n-1) (xsf())

let rec naturalNumbers n = Cons (n, fun () -> naturalNumbers (n+1))

let rec applyFilters stream = function
    | [] -> stream
    | f::fs -> applyFilters (filter (fun y -> y % f = 0) stream) fs


let naturalSequence = Seq.initInfinite( fun n -> n )
let naturalEnumerator = naturalSequence.GetEnumerator()
naturalEnumerator.MoveNext() |> ignore

let layeredFilter = function
    | [] -> failwith "Empty list"
    | x::y::z::w::[] -> (fun n -> n % x = 0 && n % y = 0 && n % z = 0 && n % w = 0)
    | _ -> failwith "Expecting four filters"
    
let rec moveNextFilter fs = 
    naturalEnumerator.MoveNext() |> ignore
    if (layeredFilter fs) naturalEnumerator.Current then naturalEnumerator.Current
    else moveNextFilter fs

let takeFilteredSeq fs n =
    let rec loop acc = function
        | 0 -> List.rev acc
        | n -> loop ((moveNextFilter fs)::acc) (n-1)
    loop [] n

let moveFilteredSeq fs n =
    for i = 1 to n do moveNextFilter fs |> ignore


// P8 - Create a tail-recursive function that has a big integer as input and calculates 2I 
// raised to that power.
// Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.

let exponential n =
    let rec loop acc n = 
        if n = 0I then 1I
        elif n = 1I then acc
        else loop (2I * acc) (n - 1I) 
    loop 2I n


// P11 - Write a non-recursive fibonacci function using imperative F#.
// Compare the timing with a tail-recursive fibonacci.

let imperativeFib n = 
    let curr = ref 1
    let prev = ref 0
    
    if n = 0 then 0
    else
        for i = 0 to n - 2 do
            let temp = !curr + !prev
            prev := !curr
            curr := temp
        !curr

let functionalFib n = 
    let rec loop curr prev = function
        | 0 -> 0
        | 1 -> curr
        | n -> loop (curr + prev) curr (n-1)
    loop 1 0 n
        

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
    
    let Program_A = parse_P3 ""
    let Program_B = parse_P3 "ab|ba"
    let Program_C = parse_P3 "aac|baa"
    let Program_D = parse_P3 "aba|baa"
    let Program_E = parse_P3 "bab|bab"

    printfn "Part B:"
    printfn "parser '' = %A" <| Program_A
    printfn "parser 'ab|ba' = %A" <| Program_B
    printfn "parser 'aac|baa' = %A \n" <| Program_C
    printfn "parser 'aba|baa' = %A \n" <| Program_D
    printfn "parser 'bab|bab' = %A \n" <| Program_E

    printfn "Part C:"
    printf "Program A -> " 
    try syntax_P3 Program_A with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program B -> " 
    try syntax_P3 Program_B with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program C -> " 
    try syntax_P3 Program_C with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program D -> " 
    try syntax_P3 Program_D with | Failure(e) -> printfn "Syntax error: %s" e     
    printf "Program E -> " 
    try syntax_P3 Program_E with | Failure(e) -> printfn "Syntax error: %s" e 
    printfn ""

    printfn "Part D:"
    printf "Program A -> " 
    try syntax_tree_P3 Program_A with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program B -> " 
    try syntax_tree_P3 Program_B with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program C -> " 
    try syntax_tree_P3 Program_C with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program D -> " 
    try syntax_tree_P3 Program_D with | Failure(e) -> printfn "Syntax error: %s" e 
    printf "Program E -> " 
    try syntax_tree_P3 Program_E with | Failure(e) -> printfn "Syntax error: %s" e 
    printfn ""

    printfn "Full example:"
    printfn "syntax_check (parse 'aabab|babaa') ="
    syntax_tree_P3 (parse_P3 "aabab|babaa")
    printfn ""

    printfn "\n"

    //  -------------------

    printfn "Problem 5\n"
    
    printfn "interleave [] [] = %A" <| interleave [] []
    printfn "interleave [3;5;7] [4;6;8] = %A" <| interleave [3;5;7] [4;6;8]
    printfn "interleave [1..6] [3..9] = %A" <| interleave [1..6] [4..9]
    printfn "interleave ['how'; 'you'] ['are'; 'doing'] = %A" <| interleave ["how"; "you"] ["are"; "doing"]
  
    printfn "\n"

    //  -------------------
    
    printfn "Problem 6\n"
    
    for i = 1 to 5 do
        alternatingSeqEnum.MoveNext() |> ignore
    printfn "infinite sequence 5th number = %A" <| alternatingSeqEnum.Current

    for i = 1 to 10 do
        alternatingSeqEnum.MoveNext() |> ignore
    printfn "infinite sequence 15th number = %A" <| alternatingSeqEnum.Current

    printfn "infinite stream 5th number = %A" <| getNth alternatingStream 5
    printfn "infinite stream 15th number = %A" <| getNth alternatingStream 15

    printfn "\n"
    
    //  -------------------
    
    printfn "Problem 7\n"
    
    printfn "Part A:"
    printfn "infinite stream [2;3;21;10] = %A \n" <| take 6 (applyFilters (naturalNumbers 1) [2;3;21;10])

    printfn "Part B:"
    printfn "ininite stream from 20th to 30th = %A \n" <| (applyFilters (naturalNumbers 1) [2;3;21;10] |> drop 20 |> take 10)
    
    printfn "Part C:"
    printfn "infinite sequence [2;3;21;10] = %A" <| takeFilteredSeq [2;3;21;10] 6
    
    moveFilteredSeq [2;3;21;10] 14
    printfn "infinite sequence from 20th to 30th = %A \n" <| takeFilteredSeq [2;3;21;10] 10 

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

    printfn "Problem 11\n"
    
    for i = 0 to 6 do 
        printfn "imperativeFib %A = %A" i (imperativeFib i)
    printfn ""
    
    for i = 0 to 6 do 
        printfn "functionalFib %A = %A" i (functionalFib i)
    printfn ""
    
    let timeImperative = System.Diagnostics.Stopwatch.StartNew()
    for i = 0 to 50 do
        imperativeFib i |> ignore
    timeImperative.Stop()
    
    let timeFunctional = System.Diagnostics.Stopwatch.StartNew()
    for i = 0 to 50 do
        functionalFib i |> ignore
    timeFunctional.Stop()

    printfn "Execution time imperativeFib (from 0 to 50) = %f ms." timeImperative.Elapsed.TotalMilliseconds
    printfn "Execution time functionalFib (from 0 to 50) = %f ms." timeFunctional.Elapsed.TotalMilliseconds 
    


    

  
    printfn "\n"

    //  -------------------
    

    Console.ReadKey() |> ignore
    0
