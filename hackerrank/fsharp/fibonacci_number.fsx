open System


let fib n =
    let rec helper prev curr = function
        | i when i < n -> helper curr (prev + curr) (i + 1)
        | _ -> curr
    helper 1 0 1

[<EntryPoint>]
let main argv = 
    let n = int <| Console.ReadLine () 
    printf "%d" <| fib n
    0 

