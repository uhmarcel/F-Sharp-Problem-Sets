// Rewriting List Library
// The purpose of this project is to rewrite List functions to learn more
// about recursion and F#.

open System
open System.Runtime.Serialization

let myListHead = function
    | [] -> []
    | x::_ -> x

let myListTail = function
    | [] -> []
    | _::xs -> xs

let rec myListMap f = function
    | [] -> []
    | x::xs -> f x :: (myListMap f xs)

let inline myListSum xs = 
    let rec aux = function
        | [] -> LanguagePrimitives.GenericZero
        | x::xs -> x + aux xs
    aux xs

let rec myListAppend xs ys = 
    match xs, ys with
        | [], [] -> []
        | [], y::ys -> y :: myListAppend [] ys
        | x::xs, ys -> x :: myListAppend xs ys

let rec myListMap2 f xs ys = 
    match xs, ys with
        | [], _ | _, [] -> []
        | x::xs, y::ys -> f x y :: myListMap2 f xs ys

let rec myListReduce f = function
    | [] -> failwith "Can't apply function to empty list"
    | [x] -> x 
    | x::xs -> f x (myListReduce f xs) 

let inline myListLength xs = 
    let rec aux = function
        | [] -> LanguagePrimitives.GenericZero
        | _::xs -> LanguagePrimitives.GenericOne + aux xs
    aux xs

let inline myListAverage xs =
    myListSum xs / myListLength xs

let rec myListCollect f = function
    | [] -> []
    | x::xs -> f x @ myListCollect f xs

let rec myListConcat = function
    | [] -> []
    | x::xs -> x @ myListConcat xs

let rec myListItem n = function
    | [] -> failwith "Out of bounds"
    | x::_ when n = 0 -> x
    | _::xs -> myListItem (n-1) xs

let inline myListSort xs =
    let rec swapBack = function
        | [] -> []
        | x::y::xs when x > y -> y :: swapBack (x::xs) 
        | xs -> xs
    let rec sort n = function 
        | [] -> []
        | xs when n = 0 -> xs
        | xs -> swapBack xs
    sort (myListLength xs) xs

let rec myListFilter f = function
    | [] -> []
    | x::xs when f x -> x :: myListFilter f xs
    | _::xs -> myListFilter f xs

let rec myListContains e = function
    | [] -> false
    | x::_ when x = e -> true
    | _::xs -> myListContains e xs

let myListFindIndex f xs = 
    let rec findIndexAux n f = function
        | [] -> failwith "Element not found"
        | x::_ when f x -> n
        | _::xs -> findIndexAux (n+1) f xs
    findIndexAux 0 f xs

let myListPartition f xs = 
    (myListFilter f xs, myListFilter (fun e -> not (f e)) xs)

let myListUnzip xs = 
    (myListMap fst xs, myListMap snd xs)

let myListUnzip3 xs = 
    let rec myUnzip3Aux (acc1, acc2, acc3) = function
        | [] -> (acc1, acc2, acc3)
        | (a,b,c)::xs -> myUnzip3Aux (acc1 @ [a], acc2 @ [b], acc3 @ [c]) xs
    myUnzip3Aux ([],[],[]) xs

let myListIndexed xs =
    let rec indexedAux n = function
        | [] -> []
        | x::xs -> (n, x) :: indexedAux (n+1) xs
    indexedAux 0 xs
    
let myListAllPairs xs ys =
    let rec makeTuple n = function
        | [] -> []
        | x::xs -> (n,x) :: makeTuple n xs
    let rec allPairs_aux ys = function
        | [] -> []
        | x::xs -> makeTuple x ys @ allPairs_aux ys xs
    allPairs_aux ys xs
    


[<EntryPoint>]
let main argv =    
    let A = [1..5]
    let B = [4;1;5;7;9]
    let C = [1.1; 3.4; 6.2; 2.1]
    let D = [[1..3]; [2..4]; [1..2]]
    let E = [(1,2);(2,4);(5,1)]
    let F = [(1,2,3);(2,4,4);(5,1,4)]

    printfn "Rewriting List Library - Testing"
    printfn "List A: %A" A
    printfn "List B: %A" B
    printfn "List C: %A" C
    printfn "List D: %A\n" D

    let f x = x * x 
    let g x y = x * y
    let h x = [x;x]
    let z x = x % 2 = 0
    
    // Map
    printfn " List.map f A = %A" <| List.map f A
    printfn "myListMap f A = %A\n" <| myListMap f A
    
    // Sum
    printfn " List.sum A = %A" <| List.sum C
    printfn "myListSum A = %A\n" <| myListSum C

    // Length
    printfn " List.length A = %A" <| List.length A
    printfn "myListLength A = %A\n" <| myListLength A

    // Average
    printfn " List.average C = %A" <| List.average C
    printfn "myListaverage C = %A\n" <| myListAverage C

    // Append
    printfn " List.append A B = %A" <| List.append A B 
    printfn "myListAppend A B = %A\n" <| myListAppend A B

    // Map2
    printfn " List.map2 f A B = %A" <| List.map2 g A B
    printfn "myListMap2 f A B = %A\n" <| myListMap2 g A B

    // Reduce
    printfn " List.reduce f A = %A" <| List.reduce g A
    printfn "myListReduce f A = %A\n" <| myListReduce g A

    // Collect
    printfn " List.collect f A = %A" <| List.collect h A
    printfn "myListCollect f A = %A\n" <| myListCollect h A

    // Concat
    printfn " List.concat D = %A" <| List.concat D
    printfn "myListConcat D = %A\n" <| myListConcat D

    // Item
    printfn " List.item 2 A = %A" <| List.item 2 A
    printfn "myListItem 2 A = %A\n" <| myListItem 2 A

    // Sort
    printfn " List.sort B = %A" <| List.sort B
    printfn "myListSort B = %A\n" <| myListSort B

    // Filter
    printfn " List.filter f A = %A" <| List.filter z A
    printfn "myListFilter f A = %A\n" <| myListFilter z A

    // Contains
    printfn " List.contains 2 A = %A" <| List.contains 2 A
    printfn "myListContains 2 A = %A\n" <| myListContains 2 A

    // FindIndex
    printfn " List.findIndex f A = %A" <| List.findIndex z A
    printfn "myListFindIndex f A = %A\n" <| myListFindIndex z A

    // Partition
    printfn " List.partition f A = %A" <| List.partition z A
    printfn "myListPartition f A = %A\n" <| myListPartition z A

    // Unzip
    printfn " List.unzip E = %A" <| List.unzip E
    printfn "myListUnzip E = %A\n" <| myListUnzip E
    
    // Unzip3
    printfn " List.unzip3 F = %A" <| List.unzip3 F
    printfn "myListUnzip3 F = %A\n" <| myListUnzip3 F

    // Indexed
    printfn " List.indexed B = %A" <| List.indexed B
    printfn "myListIndexed B = %A\n" <| myListIndexed B

    // AllPairs
    printfn " List.allPairs A B = %A" <| List.allPairs A B
    printfn "myListAllPairs A B = %A\n" <| myListAllPairs A B

    Console.ReadKey() |> ignore
    0
