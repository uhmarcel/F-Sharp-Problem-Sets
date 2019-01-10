open System

let rec fibonacci (n : int) : int =
    if n = 0 then 0
    elif n = 1 then 1
    else fibonacci(n-1) + fibonacci(n-2) 

[<EntryPoint>]
let main argv =
    printfn "Fibonacci Sequence"
    printf "Enter kth number for fibonacci sequence: "

    let k = Console.ReadLine |> int
    
    Console.ReadKey() |> ignore
    0 // return an integer exit code
