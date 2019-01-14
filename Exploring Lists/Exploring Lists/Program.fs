
open System

[<EntryPoint>]
let main argv =
    printfn "Exploring Lists in F#"

    let A = [2;3;4]
    let B = [5;6;7;8]

    printfn "List A = %A" A
    printfn "List B = %A" B

    let oneAndA = 1 :: A
    printfn "Append to head [1 :: A] = %A" oneAndA

    let concatenated = A @ B
    printfn "Concatenation [A @ B] = %A" concatenated

    let oneAndConcat = 1 :: A @ B 
    printfn "Append 1 and concat [1 :: A @ B] = %A" oneAndConcat
    
    let C = [10..20]
    printfn "List C = %A" C
    
    let odds list = 
        List.filter (fun x -> x % 2 = 1) list

    printfn "Odd elements in C = %A" (odds C)


    Console.ReadKey() |> ignore
    0 // return an integer exit code
