let input =
    System.IO.File.ReadAllLines("inputs/02.txt")
    |> Array.map (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries))

type RPS = 
    | Rock
    | Paper
    | Scissors

module RPS = 
    let fromString = function
        | "A"
        | "X" -> Rock
        | "B"
        | "Y" -> Paper
        | "C"
        | "Z" -> Scissors

type Result = 
    | Win
    | Draw
    | Lose

module Result = 
    let fromString = function
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win

let pointsFromRound [|opponent;me|] =
    match opponent, me with
    | Rock, Rock -> 1 + 3
    | Rock, Paper -> 2 + 6
    | Rock, Scissors -> 3 + 0
    | Paper, Rock -> 1 + 0
    | Paper, Paper -> 2 + 3
    | Paper, Scissors -> 3 + 6
    | Scissors, Rock -> 1 + 6
    | Scissors, Paper -> 2 + 0
    | Scissors, Scissors -> 3 + 3

input
|> Array.sumBy (Array.map RPS.fromString >> pointsFromRound)
// 14069


input
|> Array.sumBy (fun ([|opponent; result|]) ->
    match RPS.fromString opponent, Result.fromString result with
    | Rock, Win -> 2 + 6
    | Rock, Draw -> 1 + 3
    | Rock, Lose -> 3 + 0
    | Paper, Win -> 3 + 6
    | Paper, Draw -> 2 + 3
    | Paper, Lose -> 1 + 0
    | Scissors, Win -> 1 + 6
    | Scissors, Draw -> 3 + 3
    | Scissors, Lose -> 2 + 0
)
// 12411