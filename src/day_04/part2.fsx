open System

type Pos = int * int

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
    [ for dr in -1 .. 1 do
        for dc in -1 .. 1 do
            if dr <> 0 || dc <> 0 then
                yield (dr, dc) ]

let add (r, c) (dr, dc) = (r + dr, c + dc)

let countNeighbors (points: Set<Pos>) pos =
    neighbors
    |> List.filter (fun delta -> Set.contains (add pos delta) points)
    |> List.length

let rec totalRemoved points removed =
    let toRemove = points |> Set.filter (fun p -> countNeighbors points p < 4)
    if Set.isEmpty toRemove then
        removed
    else
        let remaining = Set.difference points toRemove
        totalRemoved remaining (removed + Set.count toRemove)

let buildSet (lines: string list) =
    lines
    |> List.mapi (fun r row ->
        row
        |> Seq.mapi (fun c ch -> if ch = '@' then Some (r, c) else None)
        |> Seq.choose id
        |> Seq.toList)
    |> List.concat
    |> Set.ofList

let input =
    readLines ()
    |> List.filter (fun line -> line <> "")

let result =
    input
    |> buildSet
    |> fun points -> totalRemoved points 0

printfn "%d" result
