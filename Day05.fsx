open System
open System.Collections.Generic
let input = System.IO.File.ReadAllLines("inputs/05.txt")

let parseMove (line : string) =
    let values = line.Split(" ")
    int values[1], int values[3] - 1, int values[5] - 1

let moves, chart =
    input
    |> Array.partition (fun x -> x.StartsWith("move"))
    |> fun (moves, chart) -> moves |> Array.map parseMove, chart |> Array.rev |> Array.skip 2

let stackAmount = 
    chart |> Array.head |> fun x -> (x.Length |> float) / 4. |> round |> int

let lines =
    chart
    |> Array.map(Seq.chunkBySize 4 >> Seq.mapi (fun i x -> match x |> Seq.item 1 with ' ' -> i, None | x -> i, Some x))

let initStacks lines =
    let stacks =
        Array.init stackAmount (fun _ -> Stack<char>())

    // Initialize stacks
    lines
    |> Array.fold (fun (current : Stack<char> array) line ->
        line
        |> Seq.iter (fun (index, value) ->
            value
            |> Option.iter (fun v -> current[index].Push(v))
        )
        current
    ) stacks

moves
|> Array.fold (fun (current : Stack<char> array) (count, fromStack, toStack) ->
    [|1..count|]
    |> Array.iter (fun _ -> current[toStack].Push(current[fromStack].Pop()))
    current
) (initStacks lines)
|> Array.map (fun stack -> stack.Pop())
|> String.Concat
|> printfn "Part 1: %s"

moves
|> Array.fold (fun (current : Stack<char> array) (count, fromStack, toStack) ->
    [|1..count|]
    |> Array.map (fun _ -> current[fromStack].Pop())
    |> Array.rev
    |> Array.iter (fun c -> current[toStack].Push(c))
    current
) (initStacks lines)
|> Array.map (fun stack -> stack.Pop())
|> String.Concat
|> printfn "Part 2: %s"