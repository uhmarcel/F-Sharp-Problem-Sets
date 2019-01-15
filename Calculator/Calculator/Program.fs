
open System

// Calculator operation functions
let sum a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b

// Function to format the output (e.g 4 x 6 = 24)
let formatOutput (a: int) (b: int) (operator: string) (result: int) =
    (string) a + " " + operator + " " + (string) b + " = " + (string) result

// User selection to exit the program
let EXIT = 5

[<EntryPoint>]
let main argv =
    printfn "Calculator in F# \n"
    printfn "Enter operation:"
    printfn "1 - Add"
    printfn "2 - Subtract"
    printfn "3 - Multiply"
    printfn "4 - Divide"
    printfn "5 - Exit \n"

    // Loop the selection until user chooses exit option
    while true do
        printf "Option: "
        let selection = Console.ReadLine()
        if selection |> int = EXIT then exit 0
   
        printf "Enter first operand: "
        let a = Console.ReadLine() |> int

        printf "Enter second operand: "
        let b = Console.ReadLine() |> int
    
        // Match user selection to the proper function
        let result =
            match selection with
            | "1" -> formatOutput a b "+" (sum a b)
            | "2" -> formatOutput a b "-" (sub a b)
            | "3" -> formatOutput a b "x" (mul a b)
            | "4" -> formatOutput a b "/" (div a b)
            | _ -> "Invalid selection"

        // Display result
        printfn "Result: %s \n" result

    Console.ReadKey() |> ignore
    0
