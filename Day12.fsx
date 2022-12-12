let input = System.IO.File.ReadAllLines("inputs/12.txt")

type Coord =
    { x : int
      y : int }

type Node = 
    { rc : Coord
      mutable value : int
      mutable unvisited: bool
      mutable distance: int }

let tryFindIndex char inp = 
    inp
    |> Array.map (fun x -> (x |> Seq.tryFindIndex (fun c -> c = char)))
    |> Array.indexed
    |> Array.find (fun (i,x) -> x.IsSome) |> fun (i, x) -> i, x.Value

let startX, startY = input |> tryFindIndex 'S'
let endX, endY = input |> tryFindIndex 'E'

let getMap() = 
    input
    |> Array.map (Seq.toArray >> Array.map int)
    |> array2D
    |> Array2D.mapi (fun x y v -> { rc = {x = x; y = y}; value = v; unvisited = true; distance = System.Int32.MaxValue})


let neighborValue ({x = x; y = y}) = 
    [x - 1, y; x + 1, y; x, y - 1; x, y + 1]

let solver startX startY =
    try
        let map = getMap()

        let maxX = map |> Array2D.length1
        let maxY = map |> Array2D.length2
        let getPossibleNeighbors (current : Node) =
            let neighborCoords = current.rc |> neighborValue |> List.filter (fun (x, y) -> x >= 0 && x < maxX && y >= 0 && y < maxY)
            neighborCoords |> List.choose (fun (x, y) ->
                let node = map[x,y]
                if node.value <= current.value + 1 then Some node
                else None
            )

        let destinationNode = map[endX, endY]

        let mutable current = map[startX, startY]
        current.distance <- 0
        current.value <- int 'a'
        destinationNode.value <- int 'z'

        while destinationNode.unvisited = true do
            for n in (current |> getPossibleNeighbors) do 
                if n.unvisited then
                    let newDist = current.distance + 1
                    if newDist < n.distance then
                        n.distance <- newDist
            current.unvisited <- false
            if destinationNode.unvisited = true then
                let newNode = map |> Seq.cast<Node> |> Seq.filter (fun (x : Node) -> x.unvisited && x.distance < System.Int32.MaxValue) |> Seq.minBy (fun node -> node.distance)
                current <- newNode
        destinationNode.distance
    with
    | _ -> System.Int32.MaxValue

solver startX startY |> printfn "Part 1: %i"

let possibleStartIndexes = 
    input
    |> Array.mapi (fun i x -> x |> Seq.toArray |> Array.mapi (fun j c -> if c = 'a' then Some (i,j) else None))
    |> Array.collect id
    |> Array.choose id

possibleStartIndexes
|> Array.map (fun (x, y) -> solver x y)
|> Array.min
|> printfn "Part 2: %i"