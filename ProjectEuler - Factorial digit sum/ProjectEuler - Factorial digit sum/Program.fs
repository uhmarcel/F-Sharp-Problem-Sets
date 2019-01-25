// Project Euler - Problem 20
// Factorial digit sum
// Find the sum of the digits in the number 100!

open System

let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial (n-1)

[<EntryPoint>]
let main argv =
    let test = 5
    printfn "Factorial(%i) = %i" test (factorial test)
    Console.ReadKey() |> ignore
    0
