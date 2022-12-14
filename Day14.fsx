// Well, this is slow.. Some mutability might help here. Everything to array2D?
let input = System.IO.File.ReadAllLines("inputs/14.txt")

let getRocks =
    let steps a b = [a..(if a <= b then 1 else -1)..b]
    Array.pairwise
    >> Array.fold (fun acc ((x1, y1), (x2, y2)) -> acc @ List.allPairs (steps x1 x2) (steps y1 y2)) []
    >> List.distinct
    >> List.toArray

let rocks = 
    input
    |> Array.map (fun x -> x.Split([|' '; '-';'>';','|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (Array.map int >> Array.chunkBySize 2 >> Array.map (fun a -> a[0], a[1]))
    |> Array.collect getRocks
    |> Set.ofArray

let sandPour = 500,0
let maxY = rocks |> Seq.maxBy snd |> snd

let rec addSand (x, y) floor obstacles =
    if (not floor && y >= maxY + 2) || obstacles |> Set.contains(x, y) then None
    else
        if floor && y >= maxY + 1 then
            let obs = obstacles |> Set.add (x, y)
            Some(obs, obs)
        else
            match obstacles |> Set.contains(x, y + 1), obstacles |> Set.contains(x - 1, y + 1), obstacles |> Set.contains(x + 1, y + 1) with
            | false, _, _ -> addSand (x, y + 1) floor obstacles
            | _, false, _ -> addSand (x - 1, y + 1) floor obstacles
            | _, _, false -> addSand (x + 1, y + 1) floor obstacles
            | _ ->
                let obs = obstacles |> Set.add (x, y)
                Some(obs, obs)

Seq.unfold (addSand sandPour false) rocks
|> Seq.length
|> printfn "Part 1: %i"

Seq.unfold (addSand sandPour true) rocks
|> Seq.length
|> printfn "Part 2: %i"