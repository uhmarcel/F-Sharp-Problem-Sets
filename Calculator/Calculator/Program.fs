
open System

// Calculator operation functions
let sum a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b

// Curried function, returns curried operation chosen by user
let calcFunction operation =  
    match operation with
    | "1" -> sum
    | "2" -> sub
    | "3" -> mul
    | "4" -> div
    | _ -> exit

// Returns the operation's symbol for display
let getOperatorSymbol operation =
    match operation with
    | "1" -> "+"
    | "2" -> "-"
    | "3" -> "x"
    | "4" -> "/"
    | _ -> "?"

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
    
        // Display result
        printfn "Result: %s \n" (formatOutput a b (getOperatorSymbol selection) (calcFunction selection a b))

    Console.ReadKey() |> ignore
    0
