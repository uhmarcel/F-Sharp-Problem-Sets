// Project Euler - Problem 20
// Factorial digit sum
// Find the sum of the digits in the number 100!

open System

let rec factorial (n: bigint) =
    match n with
    | _ when n = 0I || n = 1I -> 1I
    | _ -> n * factorial (n - 1I)

[<EntryPoint>]
let main argv =
    printfn "ProjectEuler - Problem 20"
    printfn "Factorial digit sum"
    printfn "Find the sum of the digits in the number 100! \n"

    let x = 100I
    printfn "Factorial(%A) = %A" x (factorial x)

    Console.ReadKey() |> ignore
    0
