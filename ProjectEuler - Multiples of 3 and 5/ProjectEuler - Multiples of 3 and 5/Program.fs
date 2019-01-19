open System

let isMultThreeAndFive a = 
    (a % 3 = 0) || (a % 5 = 0)
 
[<EntryPoint>]
let main argv =
    printfn "Project Euler - Multiples of 3 and 5 \n"
    printfn "If we list all the natural numbers below 10 that are multiples of 3 or 5,"
    printfn "we get 3, 5, 6 and 9. The sum of these multiples is 23."
    printfn "Find the sum of all the multiples of 3 or 5 below 1000. \n"

    let solution = [1..999] |> List.filter isMultThreeAndFive |> List.sum

    printfn "Solution: %i" solution

    Console.ReadKey() |> ignore
    0
