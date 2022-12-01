let input = System.IO.File.ReadLines("day1_input.txt")

let chunkByElf input =
    let mutable i = 0
    input
    |> Seq.map(fun v ->
        if v = "" then i <- i + 1
        (i, v)
    )
    |> Seq.filter (fun (_, v) -> v <> "")
    |> Seq.groupBy fst
    |> Seq.map (fun (_, v) -> Seq.map snd v)

let sumCalories (elfFood:seq<string>) : int =
    elfFood
    |> Seq.map int
    |> Seq.sum

let elvesCalories =
    input
    |> chunkByElf
    |> Seq.map sumCalories

let topCalories = Seq.max elvesCalories
let topThree =
    elvesCalories
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

printfn "Part1 : max calories: %d" (topCalories)
printfn "Part2 : top three calories: %d" (topThree)