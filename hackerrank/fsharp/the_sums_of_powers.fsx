open System


let computeReprVars X N = 
    
    let floatN = float N
    let floatX = float X

    let rec iter lower upper f X k acc =
        match (lower, upper) with
        | (lower, upper) when lower > upper -> acc
        | _ -> iter (lower+1) upper f X (lower+1) (f (X - int(float(lower) ** floatN)) k acc)
    
    let logX = int <| floatX ** (1.0/floatN)
    
    let rec countValidVars X k acc =
        match X with
        | X when X < 0 -> acc
        | X when X = 0 -> acc + 1
        | _ -> iter (k+1) logX countValidVars X (k+1) acc 
    
    countValidVars X 0 0 


[<EntryPoint>]
let main argv = 
    let X = int <| Console.ReadLine ()
    let N = int <| Console.ReadLine ()
    printfn "%d" <| computeReprVars X N 
    0

