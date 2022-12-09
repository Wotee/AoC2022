let input = System.IO.File.ReadAllLines("inputs/08.txt")

let arr = 
    input
    |> Array.map Seq.toArray 
    |> array2D
    |> Array2D.map (string >> int)

let maxSide = (arr |> Array2D.length1) - 1

List.allPairs [1..maxSide-1] [1..maxSide-1] // Sides are visible, skip them in calculation and add later
|> List.sumBy (fun (x, y) ->
    let xs = arr[x,*]
    let ys = arr[*,y]
    let revXs = xs |> Array.rev
    let revYs = ys |> Array.rev
    let value = arr[x, y]
    (xs[..y-1] |> Array.forall (fun v -> v < value)
    || revXs[..^(y+1)] |> Array.forall (fun v -> v < value)
    || ys[..x-1] |> Array.forall (fun v -> v < value)
    || revYs[..^(x+1)] |> Array.forall (fun v -> v < value))
    |> System.Convert.ToInt32
    )
|> fun x -> x + int (maxSide*4) // Add sides

let asd value x =
    x
    |> Array.tryFindIndex (fun v -> v >= value)
    |> Option.map ((+) 1)
    |> Option.defaultValue (x |> Array.length)

List.allPairs [0..maxSide] [0..maxSide]
|> List.map (fun (x, y) ->
    let xs = arr[*,x]
    let ys = arr[y,*]
    let value = arr[y,x]
    let up = xs[..y-1] |> Array.rev |> asd value
    let left = ys[..x-1] |> Array.rev |> asd value
    let down = xs[y+1..] |> asd value
    let right = ys[x+1..] |> asd value
    up * down * left * right
)
|> List.max
|> printfn "Part 2: %i"