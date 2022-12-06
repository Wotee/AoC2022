let input = System.IO.File.ReadAllText("inputs/06.txt")

let solve length = 
    Seq.windowed length
    >> Seq.findIndex(fun x -> x |> Seq.distinct |> Seq.length = length)
    >> (+) length

input 
|> solve 4
|> printfn "Part 1: %i"

input
|> solve 14
|> printfn "Part 2: %i"