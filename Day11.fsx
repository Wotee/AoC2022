let input = System.IO.File.ReadAllText("inputs/11.txt").Split("\n\n")

let (|DivisibleBy|_|) (divisor : int) (i : int) =
    if i % divisor = 0 then Some () else None 

let (|Int|_|) (x : string) =
    match System.Int32.TryParse x with
    | true, x -> Some x
    | false, _ -> None

type Monkey = {
    Items : int list
    Operation : int -> int
    DivBy : int
    ThrowTo : int -> int
    InspectCount : uint64
}

let parseOperation ([|_;b;c|] : string array) : int -> int =
    let operation = match b with | "+" -> (+) | "*" -> (*)
    match c with
    | Int v -> fun old -> operation old v
    | _ -> fun old -> operation old old

let parseThrowTo (a:string) (b:string) (c:string) =
    let number = a.Split(" ") |> Array.last |> int
    let ifTrue = b.Split(" ") |> Array.last |> int
    let ifFalse = c.Split(" ") |> Array.last |> int
    number, function | DivisibleBy number -> ifTrue | _ -> ifFalse

let getMonkeys() =
    input
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (fun x ->
        let items = x[1].Split(":")[1] |> fun (y : string) -> y.Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun y -> y.Trim() |> int) |> Array.toList
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
    let k = monkeys |> Array.fold (fun acc monke -> acc * monke.DivBy) 1
    let inspectItems monke =
        monke.Items
        |> List.iter (fun item ->
            let newWorryLevel =
                if relief
                then monke.Operation item / 3
                else monke.Operation item % k
            let throwTo = monke.ThrowTo newWorryLevel
            let newMonke = monkeys[throwTo]
            monkeys[throwTo] <- { newMonke with Items = newMonke.Items@[newWorryLevel]}
        )
        { monke with Items = []; InspectCount = monke.InspectCount + (uint64 monke.Items.Length)}
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
// 117624
solve 10000 false |> printfn "Part 2: %i"
// 16792940265