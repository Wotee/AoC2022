open System
let input = System.IO.File.ReadAllLines("inputs/05.txt")

let parseMove (line : string) =
    let values = line.Split(" ")
    int values[1], int values[3] - 1, int values[5] - 1

let moves, chart =
    input
    |> Array.partition (fun x -> x.StartsWith("move"))
    |> fun (moves, chart) -> moves |> Array.map parseMove, chart |> Array.rev |> Array.skip 2

let values() =
    chart
    |> Array.map (Seq.chunkBySize 4 >> Seq.mapi (fun i x -> match x |> Seq.item 1 with ' ' -> None | x -> Some (i, x)) >> Seq.choose id)
    |> Seq.collect id
    |> Seq.groupBy fst
    |> Seq.map (
        snd
        >> Seq.map snd
        >> Seq.toList)
    |> Seq.toArray

let solve values order =
    Array.fold (fun (acc : list<char> array) (a, f, t) ->
        let move = acc[f][^(a-1)..] |> order
        acc[f] <- acc[f][..^a]
        acc[t] <-  acc[t]@move
        acc
    ) values
    >> Array.map (List.last)
    >> String.Concat

moves |> solve (values()) List.rev |> printfn "Part 1: %s"
moves |> solve (values()) id |> printfn "Part 2: %s"