
open System


// P1 - Discriminated Unions

// a) Create a discriminated union for Coordinates that can be a Tuple, Threeple
//    or Fourple that represent tuples of size two, three and four. 
//    The type for the union should be polymorphic.

type Coordinates<'a> =
    | Tuple of 'a * 'a
    | Threeple of 'a * 'a * 'a
    | Fourple of 'a * 'a * 'a * 'a


// b) Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.

let A = Tuple (2, 6)
let B = Threeple (1.3, 4.2, 6.3)
let C = Fourple ("and", "or", "not", "xor")


// c) Create a function that has a parameter of a binary function and Coordinate. 
//    Apply the function to the Coordinate like List.reduce.

let rec coordinateReduce f = function
    | Tuple (r,s) -> f r s
    | Threeple (r,s,t) -> Tuple (f r s, t) |> coordinateReduce f
    | Fourple (r,s,t,v) -> Threeple (f r s, t, v) |> coordinateReduce f


// d) Call the function with (+) for each of the Coordinates in part (b).

let P1D_Result = (
    coordinateReduce (+) A,
    coordinateReduce (+) B,
    coordinateReduce (+) C
    )


// Call the function with (-) for the numeric Coordinates in part (b). 
// Be sure that your function implements the normal associativity for (-).

let P1E_Result = (
    coordinateReduce (-) A,
    coordinateReduce (-) B
    )




[<EntryPoint>]
let main argv =
    printfn "Problem Set 2 Workbench"
    printfn "%A" P1D_Result
    printfn "%A" P1E_Result
    Console.ReadKey() |> ignore
    0
    
    