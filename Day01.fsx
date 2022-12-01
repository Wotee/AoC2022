let input = System.IO.File.ReadAllText("inputs/01.txt")

let values =
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.sumBy int64)
    |> Array.sortDescending

values
|> Array.head
|> printfn "Part 1: %i"

values
|> Array.take 3
|> Array.sum
|> printfn "Part 2: %i"