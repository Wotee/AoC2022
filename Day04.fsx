let inp =
    System.IO.File.ReadAllLines("inputs/04.txt")
    |> Array.map (fun row -> row.Split[|',';'-'|] |> Array.map int)

inp
|> Array.sumBy (fun [|a;b;c;d|] -> System.Convert.ToInt32((a <= c && b >= d) || (a >= c && b <= d)))
|> printfn "Part 1: %i"

inp
|> Array.sumBy (fun [|a;b;c;d|] -> System.Convert.ToInt32((a <= d && b >= c) || (d >= a && c <= b)))
|> printfn "Part 2: %i"