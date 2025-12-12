open System

let readLines () =
    let reader = Console.In
    let rec loop acc =
        let line = reader.ReadLine()
        if isNull line then
            List.rev acc
        else
            loop (line :: acc)
    loop []

let parseRange (line: string) =
    match line.Split('-', 2) with
    | [| a; b |] -> (int64 a, int64 b)
    | _ -> failwithf "Invalid range: %s" line

let inAnyRange (value: int64) (ranges: (int64 * int64) list) =
    ranges
    |> List.exists (fun (lo, hi) -> value >= lo && value <= hi)

let lines = readLines ()

let splitIndex = lines |> List.tryFindIndex ((=) "")

let rangeLines, idLines =
    match splitIndex with
    | Some idx ->
        let before = lines |> List.take idx
        let after = lines |> List.skip (idx + 1)
        before, after
    | None -> lines, []

let ranges = rangeLines |> List.map parseRange

let ids =
    idLines
    |> List.filter (fun s -> s <> "")
    |> List.map int64

let freshCount =
    ids
    |> List.filter (fun value -> inAnyRange value ranges)
    |> List.length

printfn "%d" freshCount
