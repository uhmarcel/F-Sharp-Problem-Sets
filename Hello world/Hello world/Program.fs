// First program in F#
// Asks for your name and returns a message

open System

// Function to ask name and display msg
let helloWorld() =  
   
    printf "Enter your name: "      // Retrieve user name
    let name = Console.ReadLine()
    
    printfn "Hello %s from F#!" name    // Display message
    Console.ReadKey() |> ignore         // Wait for key before exit

helloWorld()

