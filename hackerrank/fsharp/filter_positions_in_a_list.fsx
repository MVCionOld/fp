open System


let readList streamRead typeCast =
    Seq.initInfinite (fun _ -> streamRead ())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map typeCast
    |> List.ofSeq

let filterPositions list =
    list
    |> List.indexed
    |> List.filter (fun (idx,_) -> idx % 2 <> 0)
    |> List.map (fun (_,item) -> item)

let printList list =
    list
    |> List.map (printfn "%d")

[<EntryPoint>]
let main argv = 
    let list = readList Console.ReadLine int
    let filteredList = filterPositions list
    printList filteredList |> ignore
    0 

