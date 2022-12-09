let input = System.IO.File.ReadAllLines("inputs/03.txt")

let solve etl =
    etl >> Seq.sumBy (Set.intersectMany >> Seq.sumBy (fun (c : char) -> (int c - 38) % 58))

input
|> solve (Seq.map (Seq.splitInto 2 >> Seq.map Set.ofSeq))
|> printfn "Part 1: %i"

input
|> solve (Seq.chunkBySize 3 >> Seq.map (Seq.map Set.ofSeq))
|> printfn "Part 2: %i"