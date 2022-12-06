let input = System.IO.File.ReadAllText("inputs/06.txt")

let solve len = 
    Seq.windowed len
    >> Seq.findIndex(Seq.distinct >> Seq.length >> (=) len)
    >> (+) len

input 
|> solve 4
|> printfn "Part 1: %i"

input
|> solve 14
|> printfn "Part 2: %i"