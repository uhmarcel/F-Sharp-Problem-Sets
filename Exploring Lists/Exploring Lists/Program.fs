
open System

[<EntryPoint>]
let main argv =
    printfn "Exploring Lists in F# \n"

    let A = [2;3;4]
    let B = [5;6;7;8]

    printfn "List A = %A" A
    printfn "List B = %A" B
    printfn ""

    let oneAndA = 1 :: A
    printfn "Append to head [1 :: A] = %A" oneAndA

    let concatenated = A @ B
    printfn "Concatenation [A @ B] = %A" concatenated

    let oneAndConcat = 1 :: A @ B 
    printfn "Append 1 and concat [1 :: A @ B] = %A" oneAndConcat
    printfn ""

    let C = [10..20]
    printfn "List C = %A" C
    printfn ""

    let odds list = 
        List.filter (fun x -> x % 2 = 1) list

    printfn "Odd elements in C = %A" (odds C)
    printfn "Sum of elements in C = %i" (List.sum C)
    printfn "Sum of odd elements in C = %i" (List.sum (odds C))

    let square element = element * element
    let squareList list = List.map square list  // == List.map (fun x -> x*x) list
    printfn "Elements squared in C = %A" (squareList C)

    let average list = 
        (List.sum list) / (List.length list)
    printfn "Average in C = %i" (average C)
    printfn "Average in squares of C = %i" (average (squareList C))

    Console.ReadKey() |> ignore
    0 // return an integer exit code
