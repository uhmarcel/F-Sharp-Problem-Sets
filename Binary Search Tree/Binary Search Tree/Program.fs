
open System

type 'a Tree =
    | Br of 'a * 'a Tree * 'a Tree
    | Lf

let rec minimum = function
    | Br (n, Lf, _) -> n
    | Br (_, t1, _) -> minimum t1
    | Lf -> failwith "Tree has no elements"

let rec maximum = function
    | Br (n, _, Lf) -> n
    | Br (_, _, t2) -> maximum t2
    | Lf -> failwith "Tree has no elements"

let rec find e = function
    | Lf -> false
    | Br (n, t1, t2) -> if e = n then true
                        elif e < n then find e t1
                        else find e t2

let rec insert e = function
    | Lf -> Br (e, Lf, Lf)
    | Br (n, t1, t2) -> if e < n then Br (n, insert e t1, t2)
                        else Br (n, t1, insert e t2)


[<EntryPoint>]
let main argv =

    printfn "Binary Search Trees \n"

    let A = Br(9, Br(7, Br(2, Lf, Br(3, Lf, Lf)), Br(8, Lf, Lf)), Br(12, Br(11, Lf, Lf), Br(16, Lf, Lf)))
    let B = Br(6, Lf, Lf)

    printfn "Tree A: \n %A \n" A
    printfn "Tree B: \n %A \n" B

    printfn "minimum A = %A" <| minimum A
    printfn "maximum A = %A\n" <| maximum A

    printfn "find 11 A = %A" <| find 11 A
    printfn "find 11 B = %A\n" <| find 11 B

    printfn "insert 8 B = %A" <| insert 8 B 
    printfn "insert 2 B = %A" <| insert 2 B
    printfn "B |> insert 7 |> insert 12 = %A" <| (B |> insert 7 |> insert 12)






    
    Console.ReadKey() |> ignore
    0 // Return exit code
