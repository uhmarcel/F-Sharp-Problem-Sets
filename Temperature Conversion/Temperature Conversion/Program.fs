open System

let toCelsius(temp) = 
    (temp - 32) * 5 / 9

let toFahrenheit(temp) =
    (temp * 5 / 9) + 32

let INVALID_SELECTION = -1

[<EntryPoint>]
let main argv =
    printfn "Temperature Conversion Program"
    
    printf "Enter a temperature to convert: "
    let temperature = Console.ReadLine() |> int

    printf "Enter the temperature unit ('f' for Fahrenheit / 'c' for Celsius): "
    let unit = Console.ReadLine()

    let output = 
        if unit = "f" then toCelsius(temperature)  
        elif unit = "c" then toFahrenheit(temperature)
        else INVALID_SELECTION
    
    printfn "Converted temperature: %d" output

    Console.ReadKey() |> ignore
    0
