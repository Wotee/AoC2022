let rounds =
    System.IO.File.ReadAllLines("inputs/02.txt")
    |> Array.map (fun s -> (int s[0] - int 'A', int s[2] - int 'X'))

rounds
|> Array.sumBy (fun (opp, you)  -> you + 1 + ((you - opp + 4) % 3) * 3)
|> printfn "Part 1: %i"

rounds
|> Array.sumBy (fun (opp, res) -> ((opp + res + 2) % 3) + 1 + res * 3)
|> printfn "Part 2: %i"