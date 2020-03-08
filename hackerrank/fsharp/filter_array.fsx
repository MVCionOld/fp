open System 


let filterArray (delim : int) =
    Seq.initInfinite (fun _ -> System.Console.ReadLine ())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map (fun x -> x |> int)
    |> Seq.filter (fun x -> x < delim)
    |> List.ofSeq
    |> List.iter (fun x -> printf "%d\n" x)

[<EntryPoint>]
let main argv =
    let delim : int = Console.ReadLine () |> int
    filterArray delim
    0
