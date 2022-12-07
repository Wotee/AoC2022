let input = System.IO.File.ReadAllLines("inputs/07.txt")

let (|Size|_|) (x : string) =
    match System.UInt64.TryParse x with
    | true, v -> Some (Size v)
    | false, _ -> None

let rec propagateFileSize directories path size = 
    match path with
    | [] -> directories
    | _::tail ->
        let dirs = directories |> Map.change path (Option.defaultValue 0UL >> (+) size >> Some)
        propagateFileSize dirs tail size

let directories = 
    input
    |> Array.fold (fun (path: string list, directories: Map<string list, uint64>) line ->
        match line.Split(" ") with
        | [|"$"; "cd"; ".."|] -> path[1..], directories
        | [|"$"; "cd"; dest|] -> dest::path, directories
        | [|Size size; _|] -> path, propagateFileSize directories path size
        | _ -> path, directories
    ) ([], Map.empty)
    |> snd

directories
|> Map.values
|> Seq.filter (fun v -> v <= 100_000UL)
|> Seq.sum
|> printfn "Part 1: %i"

let [<Literal>] totalSpace = 70_000_000UL
let [<Literal>] requiredSpace = 30_000_000UL

let used = directories[["/"]]
let free = totalSpace - used
let missing = requiredSpace - free

directories
|> Map.values
|> Seq.sort
|> Seq.find (fun v -> v >= missing)
|> printfn "Part 2: %i"