open System

[<EntryPoint>]
let main argv =
    printfn "Project Euler - Sum square difference \n"
    printfn "Find the difference between the sum of the squares of the first"
    printfn "one hundred natural numbers and the square of the sum. \n"

    let sumOfSquares = [1I..100I] |> List.sum |> (fun a -> a * a) 
    let squareOfSums = [1I..100I] |> List.map (fun a -> a * a) |> List.sum
    let solution = sumOfSquares - squareOfSums

    printfn "Solution: %A" solution

    Console.ReadKey() |> ignore
    0