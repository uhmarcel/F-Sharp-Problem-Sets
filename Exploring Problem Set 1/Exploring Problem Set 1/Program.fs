open System

[<EntryPoint>]
let main argv =
    
    // Test for problem 5
    let A = [[1;2;3];[5;6;7]]
    let B = [8;9;10]
    
    printfn "Problem 5:"
    printfn "A = %A" A
    printfn "B = %A" B
    printfn  "((List.map List.head) A) @ B = %A\n" (((List.map List.head) A) @ B)

    // Test for problem 8
    let rec foo = function
        | (xs, []) -> xs
        | (xs, y::ys) -> foo (xs@[y], ys)
    let A = [1;2;3]
    let B = [7;8]

    printfn "Problem 8:"
    printfn "foo %A %A = %A\n" A B (foo (A,B))

    // Test rec format
    let rec fib = function
        | n when n = 0 -> 1
        | n when n = 1 -> 1
        | n -> fib (n-1) + fib (n-2) 
    let test = 4;

    printfn "Testing rec formats"
    printfn "Fib(%i) = %A" test (fib(test))
    Console.ReadKey() |> ignore
    0