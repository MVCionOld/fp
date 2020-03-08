open System

let rec forLoop counter condition loopStep loopBody =
    if counter |> condition then
        counter |> loopBody
        forLoop (loopStep counter) condition loopStep loopBody
        
let n : int = Console.ReadLine () |> int
forLoop 0 ((>) n) ((+) 1) (fun _ -> printfn "Hello World")

