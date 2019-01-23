// Project Euler - Problem 10
// Summation of primes
// Find the sum of all the primes below two million.

open System

let MAX = 10000

let isPrime n = 
    [2..n-1] |> List.forall (fun elem -> n % elem <> 0)

[<EntryPoint>]
let main argv =
    printfn "ProjectEuler - Problem 10"
    printfn "Summation of primes"
    printfn "Find the sum of all the primes below two million \n"

    let solution = [2..MAX] |> List.filter isPrime |> List.sum

    printfn "Sum of primes up to %i = %i" MAX solution

    Console.ReadKey() |> ignore
    0
