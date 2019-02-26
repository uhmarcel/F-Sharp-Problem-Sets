
open System

type Book = { Id:int; Title:string; Author:string; Genre:string}

let b1 : Book = {
    Id = 1234;
    Title = "Intro to F#";
    Author = "Jhon L.";
    Genre = "Computer Science"
}

let b2 : Book = {
    Id = 2322;
    Title = "SQL database";
    Author = "Henry B.";
    Genre = "Computer Science"
}

let b3 : Book = {
    Id = 4425;
    Title = "Calculus II";
    Author = "Leonard T.";
    Genre = "Mathematics"
}


[<EntryPoint>]
let main argv =  
    printfn "Hello World from F#!"
    0 // return an integer exit code
