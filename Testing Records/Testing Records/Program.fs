// Testing Records
// Simple program to showcase the use of records.
// It manipulates a list of Book records to show those with the specific 
// genre Computer Science.

open System

type Book = { Id:int; Title:string; Author:string; Genre:string}

let b1 : Book = {
    Id = 1234;
    Title = "Intro to F#";
    Author = "Jhon L.";
    Genre = "Computer Science"
}

let b2 : Book = {
    Id = 4425;
    Title = "Calculus II";
    Author = "Leonard T.";
    Genre = "Mathematics"
}

let b3 : Book = {
    Id = 2322;
    Title = "SQL database";
    Author = "Henry B.";
    Genre = "Computer Science"
}

let bookList = [b1; b2; b3]

let filterByGenre genre bs = List.filter (fun book -> book.Genre = genre) bs

[<EntryPoint>]
let main argv =  
    printfn "Testing Records\n"
    printfn "Current records:"
    printfn "%A\n" bookList

    printfn "Computer Science books:"
    printfn "%A\n" <| filterByGenre "Computer Science" bookList
    
    Console.ReadKey() |> ignore
    0