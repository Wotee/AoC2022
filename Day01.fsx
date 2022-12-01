module Array =
    let rec splitAtValue value input =
        [|
            match input |> Array.tryFindIndex((=) value) with
            | Some index ->
                let a, b = input |> Array.splitAt index
                yield a
                yield! splitAtValue value (b |> Array.tail)
            | None -> ()
        |]

let input = System.IO.File.ReadAllLines("inputs/01.txt")

let values = 
    input
    |> Array.splitAtValue ""
    |> Array.map (Array.map int64 >> Array.sum)
    |> Array.sortDescending

values
|> Array.head
|> printfn "Part 1: %i"

values
|> Array.take 3
|> Array.sum
|> printfn "Part 2: %i"