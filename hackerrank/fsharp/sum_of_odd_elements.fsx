open System


let readList streamRead typeCast =
    Seq.initInfinite (fun _ -> streamRead ())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map typeCast
    |> List.ofSeq

let rec sumOddElems list =
    let reducer acc elem =
        match elem with
        | elem when elem % 2 <> 0 -> acc + elem
        | _ -> acc
    list |> List.reduce reducer

[<EntryPoint>]
let main argv = 
    let list = readList Console.ReadLine int
    printfn "%d" <| sumOddElems list
    0 

