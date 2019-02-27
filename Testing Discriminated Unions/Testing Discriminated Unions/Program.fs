// Testing Discriminated Unions
// Program showcasing a simple discriminated union with the type weekday.
// It contains a function accepting a WeekDay union type which returns the 
// schedule availability for that day.

open System

type WeekDay = 
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

let mySchedule = function
    | Monday | Wednesday -> "Busy"
    | Tuesday | Thursday -> "Available from 12:00 pm to 5:00 pm"
    | Friday -> "Available all workday"
    | _ -> "No meeting on weekends"



[<EntryPoint>]
let main argv =
    printfn "Testing Discriminated Unions"
    printfn "My schedule on Thursday -> %A" <| mySchedule Thursday

    Console.ReadKey() |> ignore
    0 // return an integer exit code
