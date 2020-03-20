open System


let readList streamRead typeCast =
    Seq.initInfinite (fun _ -> streamRead ())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map typeCast
    |> List.ofSeq

let rec computeListLength = function
    | []            -> 0
    | item :: items -> 1 + computeListLength items

[<EntryPoint>]
let main argv = 
    let list = readList Console.ReadLine int
    printfn "%d" <| computeListLength list
    0 

