let input = System.IO.File.ReadAllLines("inputs/09.txt")

let steps = 
    input
    |> Array.map (fun x -> x.Split(" ") |> fun y -> y[0], int y[1])
    |> Array.map (fun (c, n) -> Seq.init n (fun _ -> c))
    |> Seq.collect id

let dist x1 y1 x2 y2 = 
    System.Math.Sqrt((float x1 - float x2)**2.+(float y1 - float y2)**2.) |> int

let newTailPos x1 y1 x2 y2 = 
    if dist x1 y1 x2 y2 > 1 then
        let xDir = if x2 - x1 > 0 then (+) else (-)
        let yDir = if y2 - y1 > 0 then (+) else (-)
        match x1, y1, x2, y2 with
        | xa, ya, xb, _ when xa = xb -> xa, yDir ya 1
        | xa, ya, _, yb when ya = yb -> xDir xa 1, ya
        | xa, ya, _, _ -> xDir xa 1, yDir ya 1
    else
        x1, y1

let newHeadPos x y = function | "R" -> x + 1, y | "L" -> x - 1, y | "U" -> x, y + 1 | "D" -> x, y - 1

let rec updateTails n hx hy (tails : Map<int,int*int>) =
    if tails |> Seq.length = n then
        tails
    else 
        let newTails = tails |> Map.change n (Option.map (fun (x, y) -> newTailPos x y hx hy))
        let headX, headY = newTails.Item n
        updateTails (n+1) headX headY newTails

let solve amountOfTails steps =
    steps
    |> Seq.fold (fun (Hx, Hy, tails, visited : Set<int*int>) elem ->
        let newHx, newHy = newHeadPos Hx Hy elem
        let newTails = updateTails 0 newHx newHy tails
        newHx, newHy, newTails, (visited |> Set.add newTails[amountOfTails - 1])
    ) (0,0, (Array.init amountOfTails (fun i -> i, (0,0)) |> Map.ofArray), Set.empty)
    |> fun (_, _, _, set) -> set.Count

steps |> solve 1 |> printfn "Part 1: %i"
steps |> solve 9 |> printfn "Part 2: %i"