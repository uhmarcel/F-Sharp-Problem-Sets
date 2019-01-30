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
    let A = [1;2;3]
    let B = [7;8]
    let rec foo = function
        | (xs, []) -> xs
        | (xs, y::ys) -> foo (xs@[y], ys)
    
    printfn "Problem 8:"
    printfn "foo %A %A = %A\n" A B (foo (A,B))


    // Test rec format
    let rec fib = function
        | n when n = 0 -> 0
        | n when n = 1 -> 1
        | n -> fib (n-1) + fib (n-2) 
    let test = 4;
    
    printfn "Testing rec format:"
    printfn "Fib(%i) = %A" test (fib(test))
    
    
    // Test for problem 13
    let F = fun xs -> List.map (+) xs
    printfn "Problem 13:"
    printfn "fun xs -> List.map (+) xs, where xs = %A, is %A\n" A (F A) 
        // returns a list of functions, where each increments the input parameter 
        // by the amount of the element from which it was formed


    // Test for problem 16
    printfn "Problem 16:"
    printfn "Consider a fraction a/b to be defined as (int * int)."
    printfn "Define addition and multiplication as .+ and .*"

    let rec gcd = function
        | (a, 0) -> a
        | (a, b) -> gcd (b, a % b)
    
    let simplify (x,y) = (x / gcd(x,y), y / gcd(x,y))
    let (.+) (a,b) (c,d) = (a*d + c*b, b * d) |> simplify 
    let (.*) (a,b) (c,d) = (a*c, b*d) |> simplify
    
    printfn "8/3 + 4/3 = %A" ((8,3) .+ (4,3))
    printfn "8/2 * 4/12 = %A \n" ((8,2) .* (4,12))

    // Test for problem 17
    printfn "Problem 17:"
    printfn "Create a function revlists that reverses all lists."

    let revlists = List.map List.rev
    let A = [[0;1;1];[3;2];[];[5]]
    printfn "revlists(%A) = %A\n" A (revlists A)

    // Test for problem 18
    printfn "Problem 18:"
    printfn "Create a function interleave that interleaves two lists."

    let rec interleave = function
        | [], ys -> ys
        | xs, [] -> xs
        | (x::xs, y::ys) -> x::y::interleave (xs,ys)  
    let A = [1;2;3;4]
    let B = [5;6]
    printfn "interleave%A = %A" (A,B) (interleave (A,B))
    Console.ReadKey() |> ignore
    0