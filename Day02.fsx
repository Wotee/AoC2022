let rounds =
    System.IO.File.ReadAllLines("inputs/02.txt")
    |> Array.map (fun s -> (int s[0] - int 'A', int s[2] - int 'X'))

// rock 0, paper 1, scissors 2
// you + 1 => points from your choice
// (you - opp + 4) % 3 => lose = 0, tie = 1, win = 2
rounds
|> Array.sumBy (fun (opp, you)  -> you + 1 + ((you - opp + 4) % 3) * 3)
|> printfn "Part 1: %i"

// (opp + res + 2) % 3 => your choice, same + 1 as prev
rounds
|> Array.sumBy (fun (opp, res) -> ((opp + res + 2) % 3) + 1 + res * 3)
|> printfn "Part 2: %i"