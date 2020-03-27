open System


let genFromStr str = seq { for chr in str -> chr }

let mergeSeqs seq0 seq1 = Seq.zip seq0 seq1    

[<EntryPoint>]
let main argv =
    let pStr = Console.ReadLine()
    let qStr = Console.ReadLine()
    mergeSeqs (genFromStr pStr) (genFromStr qStr)
    |> Seq.iter (fun (p, q) -> printf "%c%c" p q)
    0

