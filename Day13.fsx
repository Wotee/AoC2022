open System.Text.Json.Nodes
let input = System.IO.File.ReadAllText("inputs/13.txt")

let rec nodeCompare (l : JsonNode, r : JsonNode) : bool option =
    let compareArr (l: JsonArray, r: JsonArray) =
        let ll, rl = l.Count, r.Count
        let min = System.Math.Min(ll,rl)
        Seq.zip (l |> Seq.truncate min) (r |> Seq.truncate min)
        |> Seq.map nodeCompare
        |> Seq.tryFind (fun x -> x.IsSome)
        |> Option.defaultWith(fun _ -> if ll = rl then None else Some (ll < rl))
    match l, r with
    | :? JsonValue as lv, (:? JsonValue as rv) ->
        match lv.GetValue<int>(), rv.GetValue<int>() with
        | l, r when l = r -> None
        | l, r -> Some (l < r)
    | :? JsonArray as l, (:? JsonValue as r) -> compareArr (l, JsonArray(r.GetValue<int>()))
    | :? JsonValue as l, (:? JsonArray as r) -> compareArr (JsonArray(l.GetValue<int>()), r)
    | :? JsonArray as l, (:? JsonArray as r) -> compareArr (l, r)

let values = 
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map JsonNode.Parse |> fun [|l;r|] -> l,r)

values
|> Array.mapi (fun i -> nodeCompare >> function Some true -> i + 1 | _ -> 0)
|> Array.sum
|> printfn "Part 1: %i"

let x, y = JsonNode.Parse("[[2]]"), JsonNode.Parse("[[6]]")

let packets = 
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map JsonNode.Parse
    |> Array.append [|x;y|]
    |> Array.sortWith (fun x y -> match nodeCompare(x,y) with Some true -> -1 | _ -> 1)

let first = packets |> Array.findIndex((=) x) |> (+) 1
let second = packets |> Array.findIndex((=) y) |> (+) 1
first*second |> printfn "Part 2: %i"