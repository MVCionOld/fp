open System


let memoize f =
    let memoized = ref Map.empty
    fun arg ->
        if Map.containsKey arg !memoized
            then Map.find arg !memoized
        else
            let result = f arg
            memoized := Map.add arg result !memoized
            result
            
let rec fibMem = memoize <| fun n ->
    match n with
        | 0 -> 0
        | 1 -> 1
        | n -> (fibMem (n-1) + fibMem (n-2)) % 100000007
    
[<EntryPoint>]
let main argv =
    let testCasesNum = Console.ReadLine () |> int
    { 1 .. testCasesNum }
    |> Seq.iter (
        fun _ -> Console.ReadLine ()
                |> int
                |> fibMem
                |> (printfn "%d")
                |> ignore)
    0

