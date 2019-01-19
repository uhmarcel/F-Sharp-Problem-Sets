(*
    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
*)    

open System

let isMultipleOfN n = (fun a -> a % n = 0)
 
[<EntryPoint>]
let main argv =
    let naturalUptoThousand = [1..1000]
    let multiplesThree = naturalUptoThousand |> List.filter (isMultipleOfN 3)
    let multiplesFive = naturalUptoThousand |> List.filter (isMultipleOfN 5)
    let mainList = multiplesThree @ multiplesFive
    let solution = mainList |> List.sum

    printfn "Project Euler - Multiples of 3 and 5 \n"
    printfn "If we list all the natural numbers below 10 that are multiples of 3 or 5,"
    printfn "we get 3, 5, 6 and 9. The sum of these multiples is 23."
    printfn "Find the sum of all the multiples of 3 or 5 below 1000. \n"
    printfn "Solution: %i" solution

    Console.ReadKey() |> ignore
    0
