// Project Euler - Problem 10
// Summation of primes
// Find the sum of all the primes below two million.

open System

let MAX = 2000000I

let isPrime n = 
    [2..n-1] |> List.forall (fun elem -> elem % n <> 0)

[<EntryPoint>]
let main argv =
    let test = 6
    printfn "Is number %i prime = %A" test (isPrime test) 

    Console.ReadKey() |> ignore
    0 // return an integer exit code
