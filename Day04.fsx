let input =
    System.IO.File.ReadAllLines("inputs/04.txt")
    |> Array.map (fun s -> s.Split([|','; '-'|]))
    |> Array.map (fun [|a;b;c;d|] -> [(int a)..(int b)] |> Set.ofList, [(int c)..(int d)] |> Set.ofList)

input
|> Array.sumBy (fun (a, b) -> if a |> Set.isSubset b || b |> Set.isSubset a then 1 else 0)
|> printfn "Part 1: %i"

input
|> Array.sumBy(fun (a, b) -> if a |> Set.intersect b |> Set.isEmpty then 0 else 1)
|> printfn "Part 2: %i"