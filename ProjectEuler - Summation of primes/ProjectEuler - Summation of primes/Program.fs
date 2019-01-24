// Project Euler - Problem 10
// Summation of primes
// Find the sum of all the primes below two million.

open System

let MAX = 2000000uL

let rec sieveOfErat (list: uint64 list) (n: uint64) (limit: uint64) =
    if n = limit then 
        list
    elif not (list |> List.contains n) then
        sieveOfErat list (n+1uL) limit
    else
        list |> List.filter (fun elem -> elem = n || elem % n <> 0uL) |> sieveOfErat <|| (n+1uL, limit)
    
let primesUpTo n =
    sieveOfErat [2uL..n] 2uL (uint64 <| sqrt(float n))
    
[<EntryPoint>]
let main argv =
    printfn "ProjectEuler - Problem 10"
    printfn "Summation of primes"
    printfn "Find the sum of all the primes below two million \n"

    let solution = primesUpTo MAX |> List.sum

    printfn "Testing: %A" <| primesUpTo MAX
    printfn "Sum of primes up to %i = %i" MAX solution

    Console.ReadKey() |> ignore
    0
