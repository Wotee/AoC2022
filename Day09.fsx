let positions =
    System.IO.File.ReadAllLines("inputs/09.txt")
    |> Seq.collect (fun x -> x.Split(" ") |> fun y -> Seq.init (int y[1]) (fun _ -> y[0]))
    |> Seq.scan (fun (x, y) -> function | "R" -> x+1, y | "L" -> x-1, y | "U" -> x, y+1 | "D" -> x, y-1) (0,0)

let newTail (x2, y2) (x1, y1) = 
    let dir a b = match b - a with | 0 -> (*) | x when x < 0 -> (-) | _ -> (+)
    match System.Math.Sqrt((float x1 - float x2)**2.+(float y1 - float y2)**2.) |> int with
    | x when x > 1 -> (dir x1 x2) x1 1, (dir y1 y2) y1 1
    | _ -> x1, y1

let update (knots : Map<int,int*int>) elem = 
    let rec loop (tails : Map<int,int*int>) = function
        | n when n = (tails |> Seq.length) -> tails
        | n -> loop (tails |> Map.change n (Option.map (newTail tails[n-1]))) (n+1)
    loop (knots |> Map.change 0 (fun _ -> Some elem)) 1

let solve amountOfKnots =
    Seq.scan update (Array.init (amountOfKnots+1) (fun i -> i, (0,0)) |> Map.ofArray)
    >> Seq.map (fun knots -> knots[amountOfKnots])
    >> Set.ofSeq
    >> fun set -> set.Count

positions |> solve 1 |> printfn "Part 1: %i"
positions |> solve 9 |> printfn "Part 1: %i"