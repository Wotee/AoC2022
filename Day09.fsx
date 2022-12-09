let input = System.IO.File.ReadAllLines("inputs/09.txt")

let steps = 
    input
    |> Array.map (fun x -> x.Split(" ") |> fun y -> y[0], int y[1])
    |> Array.map (fun (c, n) -> Seq.init n (fun _ -> c))
    |> Seq.collect id

let newTailPos tailX tailY headX headY =
    match tailX, tailY, headX, headY with
    | x, y, nX, nY when x + 1 = nX && y = nY -> x, y
    | x, y, nX, nY when x = nX && y + 1 = nY -> x, y
    | x, y, nX, nY when x - 1 = nX && y = nY -> x, y
    | x, y, nX, nY when x = nX && y - 1 = nY -> x, y
    | x, y, nX, nY when x = nX && y < nY -> x, y + 1
    | x, y, nX, nY when x = nX && y > nY -> x, y - 1
    | x, y, nX, nY when y = nY && x < nX -> x + 1, y
    | x, y, nX, nY when y = nY && x > nX -> x - 1, y
    | x, y, nX, nY when y + 1 = nY && x + 1 = nX -> x, y
    | x, y, nX, nY when y + 1 = nY && x - 1 = nX -> x, y
    | x, y, nX, nY when y - 1 = nY && x + 1 = nX -> x, y
    | x, y, nX, nY when y - 1 = nY && x - 1 = nX -> x, y
    | x, y, nX, nY when y < nY && x < nX -> x + 1, y + 1
    | x, y, nX, nY when y > nY && x < nX -> x + 1, y - 1
    | x, y, nX, nY when y < nY && x > nX -> x - 1, y + 1
    | x, y, nX, nY when y > nY && x > nX -> x - 1, y - 1
    | x, y, nX, nY -> x, y

let newHeadPos Hx Hy = function
    | "R" -> Hx + 1, Hy
    | "L" -> Hx - 1, Hy
    | "U" -> Hx, Hy + 1
    | "D" -> Hx, Hy - 1

steps
|> Seq.fold (fun (Hx, Hy, Tx, Ty, visited : Set<int*int>) elem ->
    let newHX, newHY = newHeadPos Hx Hy elem
    let newTx, newTy = newTailPos Tx Ty newHX newHY
    newHX, newHY, newTx, newTy, (visited |> Set.add (newTx, newTy))
) (0,0,0,0, Set.empty)
|> fun (_, _, _, _, set) ->
    printfn "%i" set.Count

let step2Tails = 
    Array.init 9 (fun i -> i, (0,0)) |> Map.ofArray

let rec updateTails n hx hy (tails : Map<int,int*int>) =
    if tails |> Seq.length = n then
        tails
    else 
        let newTails = tails |> Map.change n (Option.map (fun (x, y) -> newTailPos x y hx hy))
        let headX, headY = newTails.Item n
        updateTails (n+1) headX headY newTails

steps
|> Seq.fold (fun (Hx, Hy, tails, visited : Set<int*int>) elem ->
    let newHx, newHy = newHeadPos Hx Hy elem
    let newTails = updateTails 0 newHx newHy tails
    newHx, newHy, newTails, (visited |> Set.add newTails[8])
) (0,0, step2Tails, Set.empty)
|> fun (_, _, _, set) ->
    printfn "%i" set.Count
