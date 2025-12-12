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

let mergeRanges ranges =
    let folder acc (start', finish') =
        match acc with
        | [] -> [ (start', finish') ]
        | (start, finish) :: rest ->
            if start' <= finish + 1L then
                (start, max finish finish') :: rest
            else
                (start', finish') :: (start, finish) :: rest
    ranges
    |> List.sortBy fst
    |> List.fold folder []
    |> List.rev

let rangeSize (a: int64, b: int64) = b - a + 1L

let lines = readLines ()

let splitIndex = lines |> List.tryFindIndex ((=) "")

let rangeLines =
    match splitIndex with
    | Some idx -> lines |> List.take idx
    | None -> lines

let ranges =
    rangeLines
    |> List.filter (fun s -> s <> "")
    |> List.map parseRange

let merged = mergeRanges ranges

let totalFresh = merged |> List.sumBy rangeSize

Console.WriteLine totalFresh
