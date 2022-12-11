let input = System.IO.File.ReadAllText("inputs/11.txt").Split("\n\n")

let (|DivisibleBy|_|) (divisor : uint64) (i : uint64) =
    if i % divisor = 0UL then Some () else None 

let (|Int|_|) (x : string) =
    match System.UInt64.TryParse x with
    | true, x -> Some x
    | false, _ -> None

type Monkey = {
    Items : uint64 list
    Operation : uint64 -> uint64
    DivBy : uint64
    ThrowTo : uint64 -> int
    InspectCount : uint64
}

let parseOperation ([|_;b;c|] : string array) : uint64 -> uint64 =
    let operation = match b with | "+" -> (+) | "*" -> (*)
    match c with
    | Int v -> fun old -> operation old v
    | _ -> fun old -> operation old old

let parseThrowTo (a:string) (b:string) (c:string) =
    let number = a.Split(" ") |> Array.last |> uint64
    let ifTrue = b.Split(" ") |> Array.last |> int
    let ifFalse = c.Split(" ") |> Array.last |> int
    number, function | DivisibleBy number -> ifTrue | _ -> ifFalse

let getMonkeys() =
    input
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (fun x ->
        let items = x[1].Split(":")[1] |> fun (y : string) -> y.Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun y -> y.Trim() |> uint64) |> Array.toList
        let n, tt = parseThrowTo x[3] x[4] x[5]
        {
            Items = items
            Operation = x[2].Split(":";" new = ")[1] |> fun (y : string) -> y.Split(" ") |> parseOperation
            ThrowTo = tt
            DivBy = n
            InspectCount = 0UL
        }
    )

let solve rounds relief = 
    let monkeys = getMonkeys()
    let k = monkeys |> Array.fold (fun acc monke -> acc * monke.DivBy) 1UL
    let inspectItems monke =
        match monke.Items with
        | [] -> monke
        | items -> 
            items
            |> List.iter (fun item ->
                let newWorryLevel =
                    if relief then
                        ((monke.Operation item) / 3UL) % k
                    else monke.Operation item % k
                let throwTo = monke.ThrowTo newWorryLevel
                let newMonke = monkeys[throwTo]
                monkeys[throwTo] <- { newMonke with Items = newMonke.Items@[newWorryLevel]}
            )
            { monke with Items = []; InspectCount = monke.InspectCount + (uint64 items.Length)}
    Array.init (rounds * monkeys.Length) (fun i -> i % monkeys.Length)
    |> Array.fold (fun (acc : Monkey array) (elem : int) ->
        acc[elem] <- inspectItems acc[elem]
        acc
    ) monkeys
    |> Array.map (fun x -> x.InspectCount)
    |> Array.sortDescending
    |> Array.take 2
    |> fun [|x; y|] -> x*y

solve 20 true |> printfn "Part 1: %i"
solve 10000 false |> printfn "Part 2: %i"