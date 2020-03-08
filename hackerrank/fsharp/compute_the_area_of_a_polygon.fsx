open System


let readPair _ = Console.ReadLine().Split " "
                  |> Seq.toList
                  |> List.map float
                  |> function
                        | x :: [y] -> (x, y)
                        | _ -> (0.0, 0.0)

let computeOrientedTrSquare currPoint nextPoint = (fst currPoint - fst nextPoint) * (snd currPoint + snd nextPoint) / 2.


[<EntryPoint>]
let main argv =
    let pointsNum = Console.ReadLine () |> int
    let firstPoint = readPair ()
    let rec computePolygonSquare pointsCounter totalPoints currSquare currPoint nextPoint =
        let totalSquare = currSquare + computeOrientedTrSquare currPoint nextPoint
        if ( < ) pointsCounter totalPoints then
            computePolygonSquare (pointsCounter + 1) totalPoints totalSquare nextPoint (readPair ())
        else (totalSquare, nextPoint)
    let almostSquare, lastPoint = computePolygonSquare 2 pointsNum 0.0 firstPoint (readPair ())
    let polygonSquare = almostSquare + computeOrientedTrSquare lastPoint firstPoint |> abs
    printfn "%f" polygonSquare
    0

