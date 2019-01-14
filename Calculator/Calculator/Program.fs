
open System

let sum a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b

[<EntryPoint>]
let main argv =
    printfn "Calculator in F#"
    printfn "Enter operation:"
    printfn "1 - Add"
    printfn "2 - Subtract"
    printfn "3 - Multiply"
    printfn "4 - Divide"
    printf "Option: "
    let selection = Console.ReadLine() |> int
    
    printf "Enter first operand: "
    let a = Console.ReadLine() |> int

    printf "Enter second operand: "
    let b = Console.ReadLine() |> int

    let result = 
        if selection = 1 then sum a b
        elif selection = 2 then sub a b
        elif selection = 3 then mul a b
        elif selection = 4 then div a b
        else -1
    
    printf "Result: %i" result
    Console.ReadKey() |> ignore
    0
