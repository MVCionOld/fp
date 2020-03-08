open System

let rec forLoop counter condition loopStep loopBody =
    if counter |> condition then
        counter |> loopBody
        forLoop (loopStep counter) condition loopStep loopBody

let rec intFactorial n =
    match n with
    | m when m > 0 -> m * (intFactorial (m - 1))
    | _ -> 1
    
let expWithTenPrecision (x : float) = [ 0 .. 9 ]
                                      |> List.sumBy (fun i -> (x ** (float i)) / (i |> intFactorial |> float))
        
let n : int = Console.ReadLine () |> int
let loopBody = fun _ -> Console.ReadLine () |> float |> expWithTenPrecision |> printfn "%f"
forLoop 0 (( > ) n) (( + ) 1) loopBody
