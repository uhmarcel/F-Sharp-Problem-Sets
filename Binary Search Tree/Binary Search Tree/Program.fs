
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

let treeColapse f tree = 
    let rec colapse_aux f n = function    
        | Lf -> Br(n, Lf, Lf)
        | Br(m, Lf, Lf) -> Br(f n m, Lf, Lf)
        | Br(m, t1, Lf) -> colapse_aux f (f n m) t1 
        | Br(m, Lf, t2) -> colapse_aux f (f n m) t2
        | Br(m, t1, t2) -> match (colapse_aux f (f n m) t1) with
                            | Br (y, _, _) -> colapse_aux f y t2
                            | _ -> failwith "Impossible combination"
    let colapse_start f = function
        | Lf -> failwith "The tree is empty"
        | Br(m, Lf, Lf) -> Br(m, Lf, Lf)
        | Br(m, t1, Lf) -> colapse_aux f m t1
        | Br(m, Lf, t2) -> colapse_aux f m t2
        | Br(m, t1, t2) -> match (colapse_aux f m t1) with
                            | Br (y, _, _) -> colapse_aux f y t2
                            | _ -> failwith "Impossible combination"
    let getNode = function
        | Br(n, _, _) -> n
        | Lf -> failwith "Impossible combination"
    colapse_start f tree |> getNode

let rec treeToList_preorder = function
    | Lf -> []
    | Br(n, t1, t2) -> n :: (treeToList_preorder t1 @ treeToList_preorder t2)

let rec treeToList_postorder = function
    | Lf -> []
    | Br(n, t1, t2) -> treeToList_postorder t1 @ treeToList_postorder t2 @ [n] // horribly inneficient

let rec treeToList_inorder = function
    | Lf -> []
    | Br(n, t1, t2) -> treeToList_inorder t1 @ (n :: treeToList_inorder t2)


let rec remove e = function
    | Lf -> Lf                              // If element is not found, ignore (return leaf)
    | Br(n, t1, Lf) when e = n -> t1        // If element to remove has no right, replace with left child
    | Br(n,t1,t2) -> 
        if e < n then Br(n, remove e t1, t2)     // If element is smaller than current, search to remove in the left tree
        elif e > n then Br(n, t1, remove e t2)   // If element is greater than current, search to remove in the right tree
        else                                     // If element is equal to current, find minimum of the right tree, put it as        
            let m = minimum t2                   // the value on this current branch, and remove the minimum
            Br(m, t1, remove m t2)
          
     

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

    printfn "treeMap (fun x -> x*2) B = %A\n" <| treeMap (fun x -> x*2) B
 
    printfn "treeColapse (+) A = %A" <| treeColapse (+) A
    printfn "treeColapse (-) A = %A" <| treeColapse (-) A
    printfn "treeColapse (*) A = %A\n" <| treeColapse (*) A

    printfn "treeToList_preorder A = %A" <| treeToList_preorder A
    printfn "treeToList_postorder A = %A" <| treeToList_postorder A
    printfn "treeToList_inorder A = %A \n" <| treeToList_inorder A

    printfn "B = %A \n" B
    printfn "remove 6 B = %A" <| remove 6 B
    printfn "remove 4 B = %A" <| remove 4 B
    printfn "B |> remove 4 |> remove 6 = %A" <| (B |> remove 4 |> remove 6)
    
    Console.ReadKey() |> ignore
    0 // Return exit code
