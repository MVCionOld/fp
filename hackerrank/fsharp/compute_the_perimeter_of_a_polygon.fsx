open System


let readPair _ = Console.ReadLine().Split " "
                  |> Seq.toList
                  |> List.map float
                  |> function
                        | x :: y :: _ -> (x, y)
                        | _ -> (0.0, 0.0)

let dist pointFrom pointTo = ((fst pointFrom - fst pointTo) ** 2.0 + (snd pointFrom - snd pointTo) ** 2.0) ** 0.5


[<EntryPoint>]
let main argv =
    let pointsNum = Console.ReadLine () |> int
    let firstPoint = readPair ()
    let rec computeChainLength pointsCounter totalPoints currLength currPoint nextPoint =
        let totalLength = currLength + dist currPoint nextPoint
        if ( < ) pointsCounter totalPoints then
            computeChainLength (pointsCounter + 1) totalPoints totalLength nextPoint (readPair ())
        else (totalLength, nextPoint)
    let chainLength, lastPoint = computeChainLength 2 pointsNum 0.0 firstPoint (readPair ())
    let polygonLength = chainLength + dist firstPoint lastPoint
    printfn "%f" polygonLength
    0

