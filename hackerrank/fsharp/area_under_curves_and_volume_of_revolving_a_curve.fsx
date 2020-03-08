open System


let pi = System.Math.PI

let readFloats () = 
    Console.ReadLine().Split(' ') 
    |> Array.toList 
    |> List.map float

let computeMonom coeff power arg = coeff * (arg ** power)

let expr pairsCoeffPower arg = 
    let summator = fun acc pairCoeffPower -> acc + (computeMonom (fst pairCoeffPower) (snd pairCoeffPower) arg)
    pairsCoeffPower 
    |> List.fold summator 0.0
  
let computeAreaVolume pairsCoeffPower lowerRange rightRange =
    let folding curr y =
      let curr, v, prev = curr
      (curr + 0.001 * y, v + 0.001 * y * y * pi, y) 
    seq { lowerRange .. 0.001 .. rightRange }
    |> Seq.map (fun arg -> expr pairsCoeffPower arg)
    |> Seq.fold folding (0.0, 0.0, 0.0)


[<EntryPoint>]
let main argv = 
    let coeffs = readFloats ()
    let powers = readFloats ()
    let limits = readFloats ()
    let pairsCoeffPower = List.zip coeffs powers
    let lowerRange, rightRange = limits.[0], limits.[1]
    let area, volume, _ = computeAreaVolume pairsCoeffPower lowerRange rightRange
    printfn "%A\n%A" area volume
    0
