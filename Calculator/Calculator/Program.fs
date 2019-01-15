
open System

let sum a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b

let formatOutput (a: int) (b: int) (operator: string) (result: int) =
    (string) a + " " + operator + " " + (string) b + " = " + (string) result

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
    printfn ""

    printf "Enter first operand: "
    let a = Console.ReadLine() |> int

    printf "Enter second operand: "
    let b = Console.ReadLine() |> int
    
    let result =
        match selection with
        | 1 -> formatOutput a b "+" (sum a b)
        | 2 -> formatOutput a b "-" (sub a b)
        | 3 -> formatOutput a b "x" (mul a b)
        | 4 -> formatOutput a b "/" (div a b)
        | _ -> "Invalid selection"
    
    printf "Result: %s" result
    Console.ReadKey() |> ignore
    0
