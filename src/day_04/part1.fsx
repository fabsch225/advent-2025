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

let neighbors =
    [ for dx in -1 .. 1 do
        for dy in -1 .. 1 do
            if dx <> 0 || dy <> 0 then
                yield (dx, dy) ]

let inBounds (grid: string list) (r, c) =
    let rows = List.length grid
    if rows = 0 then
        false
    else
        let cols = grid.Head.Length
        r >= 0 && r < rows && c >= 0 && c < cols

let tryGet (grid: string list) (r, c) =
    if inBounds grid (r, c) then
        Some grid.[r].[c]
    else
        None

let adjAtSigns grid (r, c) =
    neighbors
    |> List.filter (fun (dx, dy) -> tryGet grid (r + dx, c + dy) = Some '@')
    |> List.length

let isAccessible grid pos =
    tryGet grid pos = Some '@' && adjAtSigns grid pos < 4

let allPositions (grid: string list) =
    let rows = List.length grid
    if rows = 0 then
        []
    else
        let cols = grid.Head.Length
        [ for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                yield (r, c) ]

let grid =
    readLines ()
    |> List.filter (fun line -> line <> "")

let accessibleCount =
    allPositions grid
    |> List.filter (isAccessible grid)
    |> List.length

printfn "%d" accessibleCount
