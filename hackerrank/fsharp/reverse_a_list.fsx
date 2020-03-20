open System


let readList streamRead typeCast =
    Seq.initInfinite (fun _ -> streamRead ())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map typeCast
    |> List.ofSeq

let rec reverseList list reversed =
    match list with
    | []            -> reversed
    | item :: items -> reverseList items (item :: reversed)

let printList list =
    list
    |> List.map (printfn "%d")

[<EntryPoint>]
let main argv = 
    let list = readList Console.ReadLine int
    let reversedList = reverseList list []
    printList reversedList |> ignore
    0 

