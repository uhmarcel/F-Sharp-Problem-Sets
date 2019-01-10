open System

let rec fibonacci (n : int) : int =     // fibonacci recursive function
    if n = 0 then 0
    elif n = 1 then 1
    else fibonacci(n-1) + fibonacci(n-2) 

[<EntryPoint>]
let main argv =
    printfn "Fibonacci Sequence"
    printfn "Enter 'exit' at any moment to stop."

    while 1 = 1 do                      // continuous loop
        printf "Enter kth number for fibonacci sequence : "

        let input = Console.ReadLine()
        if input = "exit" then Environment.Exit(0)  // exit upon request
    
        let k = input |> int
        printfn "fibonacci(%i) = %i" k (fibonacci(k))   // display fibonacci number
    
    0 // return an integer exit code
