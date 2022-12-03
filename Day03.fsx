let input = System.IO.File.ReadAllLines("inputs/03.txt")

let priority (c : char) =
    match System.Char.IsUpper c with
    | true -> c |> int |> (fun x -> x - 38) // 'A' + 1
    | false -> c |> int |> (fun x -> x - 96) // 'a' + 1

let calculateTheThing : string array -> int =
    Array.map Set.ofSeq
    >> Set.intersectMany
    >> Seq.sumBy priority

let etl1 =
    Array.map (fun (line : string) ->
        let half = line.Length / 2
        [|line[..half-1]; line[half..]|]
    )

let etl2 : string array -> string array array = Array.chunkBySize 3

let solve part etl input =
    input
    |> etl
    |> Array.sumBy calculateTheThing
    |> printfn "Part %i: %i" part

input |> solve 1 etl1
input |> solve 2 etl2