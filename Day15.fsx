open System

let input =
    System.IO.File.ReadAllLines("inputs/15.txt")
    |> Array.map (fun x -> x.Split([|"Sensor at x="; ", y="; ": closest beacon is at x="|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> fun [|sx;sy;bx;by|] -> (sx, sy), (bx, by))

let manhattan (sx: int, sy: int) (bx, by) =
    Math.Abs(sx-bx) + Math.Abs(sy-by)

let row = 2000000

let coveredRowThing (sx, sy) (mDist : int) =
    let yDist = Math.Abs(sy - row)
    let dist = mDist - yDist
    [-dist..dist] |> List.map (fun newX -> newX+sx, row) |> Set.ofList

let coveredSet =
    input
    |> Array.fold (fun acc (sensor, beacon) ->
        let dist = manhattan sensor beacon
        if Math.Abs(snd sensor) + dist < row then
            acc
        else
            let known = coveredRowThing sensor dist
            acc |> Set.union known
    ) Set.empty

let knownBeacons = 
    input
    |> Array.choose (fun (_, (x, y)) ->
        match y = row with
        | true -> Some (x, y)
        | false -> None)
    |> Set.ofArray

coveredSet - knownBeacons |> Set.count |> printfn "Part 1: %i"

let minVal = 0
let maxVal = 4_000_000

let isCoveredBy (sensor, nearestBeacon) beacon =
    manhattan sensor nearestBeacon >= manhattan sensor beacon

let isCoveredByAny sensors beacon =
    sensors |> Seq.exists (fun s -> isCoveredBy s beacon)

let getBorder ((sx, sy), beacon) =
    let dist = manhattan (sx, sy) beacon + 1
    [0..dist]
    |> List.collect (fun dx ->
        let dy = dist-dx
        [sx+dx, sy+dy; sx-dx, sy+dy; sx+dx, sy-dy; sx-dx, sy-dy]
    )

input
|> Seq.collect getBorder
|> Seq.find (fun (x, y) -> minVal <= x && x <= maxVal && minVal <= y && y <= maxVal && not <| isCoveredByAny input (x, y))
|> fun (x, y) -> int64 x * 4_000_000L + int64 y 
|> printfn "Part 2: %i"