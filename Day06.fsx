let input = System.IO.File.ReadAllText("inputs/06.txt")

let solve length = 
    Seq.windowed length
    >> Seq.mapi (fun i x -> i, x |> Set.ofSeq)
    >> Seq.find (fun (_,x) -> x.Count = length)
    >> fun (i,_) -> i + length

input 
|> solve 4
|> printfn "Part 1: %i"

input
|> solve 14
|> printfn "Part 2: %i"