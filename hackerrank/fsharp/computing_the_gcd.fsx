open System


let rec gcd a = function
    | b when a % b = 0 -> b
    | b -> gcd b (a % b)

[<EntryPoint>]
let main argv = 
    let input = Console.ReadLine().Split(' ') |> Array.toList |> List.map int
    let a = input.[0]
    let b = input.[1]
    printfn "%d" <| gcd a b
    0 


