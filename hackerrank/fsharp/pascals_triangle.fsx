open System


let rec nextrow l =
    match l with
    | [] -> []
    | h :: [] -> [1]
    | h :: t -> h + t.Head :: nextrow t
 
let pascalTriangle n = List.scan(fun l i -> 1 :: nextrow l) [1] [1 .. n]


[<EntryPoint>]
let main argv =
    let k = (Console.ReadLine () |> int) - 1
    for row in pascalTriangle(k) do
        for i in row do
            printf "%s" (i.ToString() + " ")
        printfn ""
    0

