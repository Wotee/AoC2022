open System.Text.Json.Nodes
let input = System.IO.File.ReadAllText("inputs/13.txt")

let rec nodeCompare (l : JsonNode, r : JsonNode) : bool option =
    let compareArrays (l : JsonArray, r: JsonArray) =
        let ll = l |> Seq.length
        let rl = r |> Seq.length
        let min = System.Math.Min(ll, rl)
        Seq.zip (l |> Seq.truncate min) (r |> Seq.truncate min)
        |> Seq.fold (fun acc elem ->
            match acc with
            | None -> nodeCompare elem
            | x -> x
        ) None
        |> function
        | None -> if ll = rl then None else Some (ll < rl)
        | x -> x
    match l, r with
    | :? JsonValue as lv, (:? JsonValue as rv) ->
        match lv.GetValue<int>(), rv.GetValue<int>() with
        | l, r when l = r -> None
        | l, r -> Some (l < r)
    | :? JsonArray as l, (:? JsonValue as r) -> compareArrays (l, JsonArray(r.GetValue<int>()))
    | :? JsonValue as l, (:? JsonArray as r) -> compareArrays (JsonArray(l.GetValue<int>()), r)
    | :? JsonArray as l, (:? JsonArray as r) -> compareArrays(l, r)

let values = 
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map JsonNode.Parse |> fun [|l;r|] -> l,r)

values
|> Array.map nodeCompare
|> Array.mapi (fun i x -> match x with | Some true -> i+1 | Some false -> 0 | None -> failwith "")
|> Array.sum
|> printfn "Part 1: %i"

let x = JsonNode.Parse("[[2]]")
let y = JsonNode.Parse("[[6]]")

let packets = 
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (JsonNode.Parse)
    |> Array.append [|x;y|]
    |> Array.sortWith (fun x y -> match nodeCompare(x,y) with Some true -> -1 | Some false -> 1 | None -> 0)

let first = packets |> Array.findIndex((=) x) |> (+) 1
let second = packets |> Array.findIndex((=) y) |> (+) 1
first*second |> printfn "Part 2: %i"