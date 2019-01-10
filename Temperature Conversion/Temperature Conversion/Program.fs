open System

[<EntryPoint>]
let main argv =
    printfn "Temperature Conversion Program"
    printf "Select either Fahrenheit or Celsius (f/c): "
    let unit = Console.ReadLine()

    if unit = "f"
    then printfn "OK"
    else printfn "NOT OK"

    Console.ReadKey() |> ignore
    0
