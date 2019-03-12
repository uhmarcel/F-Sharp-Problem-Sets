
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

let isBST_aux n flag = function
    | Br (m, _, _) when flag = 0 -> m < n
    | Br (m, _, _) when flag = 1 -> m >= n
    | Lf -> true

let rec isBST = function
    | Lf -> true
    | Br (n, t1, t2) -> if isBST_aux n 0 t1 && isBST_aux n 1 t2 then isBST t1 && isBST t2
                        else false             

let rec treeMap f = function
    | Lf -> Lf
    | Br (n, t1, t2) -> Br (f n, treeMap f t1, treeMap f t2)

let rec colapseLeft f n = function    
    | Lf -> Lf 
    | Br(m, Lf, Lf) -> Br(f n m, Lf, Lf)
    | Br(m, t1, Lf) -> colapseLeft f (f n m) t1 
    | Br(m, Lf, t2) -> colapseRight f (f n m) t2
    | Br(m, t1, t2) -> match (colapseLeft f (f n m) t1) with
                        | Br (y, _, _) -> colapseRight f (f y  t2 


let treeColapse f = function
    | Lf -> Lf
    | Br(n, Lf, Lf) -> Br(n, Lf, Lf) 
    | Br(n, t1, Lf) -> colapseLeft f n t1
    | Br(n, Lf, t2) -> colapseRight f n t2
    | Br(n, t1, t2) -> match (colapseLeft f n t1) with
                        | Br (m, _, _) -> colapseRight f m t2 
                


[<EntryPoint>]
let main argv =

    printfn "Binary Search Trees \n"

    let A = Br(9, Br(7, Br(2, Lf, Br(3,Lf,Lf)), Br(8,Lf,Lf)), Br(12, Br(11,Lf,Lf), Br(16,Lf,Lf)))
    let B = Br(4, Br(2,Lf,Lf), Br(6,Lf,Lf))
    let C = Br(6,Lf,Lf)

    printfn "Tree A: \n %A \n" A
    printfn "Tree B: \n %A \n" B
    printfn "Tree C: \n %A \n\n" C

    printfn "minimum A = %A" <| minimum A
    printfn "maximum A = %A\n" <| maximum A

    printfn "find 11 A = %A" <| find 11 A
    printfn "find 11 B = %A\n" <| find 11 B

    printfn "insert 8 C = %A" <| insert 8 C 
    printfn "insert 2 C = %A" <| insert 2 C
    printfn "C |> insert 7 |> insert 12 = %A\n" <| (C |> insert 7 |> insert 12)

    printfn "isBST A = %A" <| isBST A
    printfn "isBST B = %A" <| isBST B
    printfn "isBST Br(5, Br(6,Lf,Lf), Lf) = %A\n" <| isBST (Br(5, Br(6,Lf,Lf), Lf))

    printfn "treeMap (fun x -> x*2) B = %A" <| treeMap (fun x -> x*2) B
 
    printfn "\n%A" <| treeColapse (fun x y -> x+y) A







    
    Console.ReadKey() |> ignore
    0 // Return exit code
