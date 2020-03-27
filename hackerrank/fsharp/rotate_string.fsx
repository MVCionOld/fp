open System
 
let rotate strList =
    let rec rotateHelper head = function
        | x :: xs ->
            seq {
                yield! xs
                yield! head
                yield x
            }
            |> Seq.iter (printf "%c")
            printf " "
            rotateHelper (head @ [x]) xs
        | [] -> printf "\n"
    rotateHelper [] strList
 
[<EntryPoint>]
let main argv =
    let testCasesNum = Console.ReadLine () |> int
    { 1 .. testCasesNum }
    |> Seq.iter (
        fun _ -> Console.ReadLine()
                |> Seq.toList
                |> rotate
                |> ignore)
    0
