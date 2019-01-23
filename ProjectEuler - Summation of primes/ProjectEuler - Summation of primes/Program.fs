// Project Euler - Problem 10
// Summation of primes
// Find the sum of all the primes below two million.

open System

let MAX = 10000

let rec sieveOfErat (list: int list) (n: int) (limit: int) =
    if n = limit then 
        list
    else
        list |> List.filter (fun elem -> elem % n <> 0) |> sieveOfErat <|| (n+1, limit)
    
let primesUpTo n =
    sieveOfErat [2..n] n
    
[<EntryPoint>]
let main argv =
    printfn "ProjectEuler - Problem 10"
    printfn "Summation of primes"
    printfn "Find the sum of all the primes below two million \n"

    let solution = primesUpTo MAX |> List.sum

    printfn "Sum of primes up to %i = %i" MAX solution

    Console.ReadKey() |> ignore
    0
