open System


let computePentagonalNum n =
    let nFloat = n |> double
    (3.0 * nFloat * nFloat - nFloat) / 2.0 |> round |> int64
    
[<EntryPoint>]
let main argv =
    let testCasesNum = Console.ReadLine () |> int
    { 1 .. testCasesNum }
    |> Seq.iter (
        fun _ -> Console.ReadLine ()
                |> int
                |> computePentagonalNum
                |> (printfn "%d")
                |> ignore)
    0

