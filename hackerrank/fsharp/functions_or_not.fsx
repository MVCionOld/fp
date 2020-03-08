open System

let rec forLoop counter condition loopStep loopBody =
    if counter |> condition then
        loopBody ()
        forLoop (loopStep counter) condition loopStep loopBody

let checkFunction _ =
    let n = Console.ReadLine () |> int
    let domain = [
        for i in 1 .. n -> Console.ReadLine().Split " "
                           |> Seq.toList
                           |> fun pair -> pair.[0]
                           |> int
    ]
    domain.Length = (List.distinct domain).Length

[<EntryPoint>]
let main argv =
    let testsNum = Console.ReadLine () |> int
    let determineFunction = fun _-> printfn "%s" (if checkFunction () then "YES" else "NO")
    forLoop 0 (( > ) testsNum) (( + ) 1) determineFunction
    0

