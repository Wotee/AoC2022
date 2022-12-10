open System
let values =
    IO.File.ReadAllLines("inputs/10.txt")
    |> Array.collect (fun x -> x.Split(" "))
    |> Array.scan (fun acc -> Int32.TryParse >> function | true, x -> acc + x | false, _ -> acc) 1

let getVal (values : int array) i = values[i-1] * i

[20;60;100;140;180;220]
|> List.sumBy (getVal values)
|> printfn "Part 1: %i"

values
|> Array.chunkBySize 40
|> Array.map (Array.mapi (fun i x -> if Math.Abs(i - x) <= 1 then "#" else " ") >> String.Concat)
|> fun res -> printfn "Part 2:"; res |> Array.iter (printfn "%s")