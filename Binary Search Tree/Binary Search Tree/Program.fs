
open System

type 'a Tree =
    | Br of 'a * 'a Tree * 'a Tree
    | Lf

let rec getMinimum = function
    | Br (n, Lf, _) -> n
    | Br (_, t1, _) -> getMinimum t1
    | Lf -> failwith "Tree has no elements"



[<EntryPoint>]
let main argv =

    printfn "Binary Search Trees \n"

    let A = Br(9, Br(7, Br(2, Lf, Br(3, Lf, Lf)), Br(8, Lf, Lf)), Br(12, Br(11, Lf, Lf), Br(16, Lf, Lf)))
    
    printfn "Tree A:"
    printfn "%A \n" A

    printfn "getMinimum A = %A" <| getMinimum A
    




    
    Console.ReadKey() |> ignore
    0 // Return exit code
