open System
 
 
let validator seq =
    let rec validatorHelper r g y b = function
        | head :: tail ->
            if abs (r - g) > 1 || abs (y - b) > 1
                then 0
            else
                if head = 'R'
                    then validatorHelper (r + 1) g y b tail
                elif head = 'G'
                    then validatorHelper r (g + 1) y b tail
                elif head = 'Y'
                    then validatorHelper r g (y + 1) b tail
                else
                    validatorHelper r g y (b + 1) tail
        | [] ->
            if r = g && y = b
                then 1
            else
                0
    validatorHelper 0 0 0 0 seq
 
[<EntryPoint>]
let main argv =
    let testCasesNum = Console.ReadLine () |> int
    { 1 .. testCasesNum }
    |> Seq.iter (
        fun _ -> Console.ReadLine()
                |> Seq.toList
                |> validator
                |> (function
                    | n when n = 0 -> printfn "False"
                    | _ -> printfn "True")
                |> ignore)
    0
