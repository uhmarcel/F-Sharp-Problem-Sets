open System

[<EntryPoint>]
let main argv =
    
    let A = [[1;2;3];[55;2;7]]
    let B = [8;9;10]

    // Test for problem 5
    let test = ((List.map List.head) A) @ B
    printfn "Test: %A" test

    Console.ReadKey() |> ignore
    0