
open System

[<EntryPoint>]
let main argv =
    printfn "Exploring Lists in F#"

    let A = [2;3;4]
    let B = [5;7;8]

    printfn "List A = %A" A
    printfn "List B = %A" B

    0 // return an integer exit code
