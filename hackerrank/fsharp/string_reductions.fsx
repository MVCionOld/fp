open System


[<EntryPoint>]
let main argv =
    let str = Console.ReadLine()
    [ for c in str -> c ]
    |> List.distinct
    |> (List.iter <| printf "%c")
    0
