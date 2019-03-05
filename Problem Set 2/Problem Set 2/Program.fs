
open System


// 1- Discriminated Union
// a) Create a discriminated union for Coordinates that can be a Tuple, Threeple
//    or Fourple that represent tuples of size two, three and four. 
//    The type for the union should be polymorphic.

type Coordinates =
    | Tuple of unit * unit
    | Threeple of unit * unit * unit
    | Fourple of unit * unit * unit * unit


[<EntryPoint>]
let main argv =
    printfn "Problem Set 2 Workbench"
    Console.ReadKey() |> ignore
    0
