open System.IO
#r "nuget: FSharpx.Collections"
#r "nuget: FSharpPlus"
open FSharpx.Collections
open FSharpPlus

let getMazeHeat defVal (i, j) = 
    Seq.tryItem i >=> Seq.tryItem j >> Option.defaultValue defVal

let inline mulTuple (i, j) k = (i * k, j * k)

let southNorth = [(1, 0); (-1, 0)]
let eastWest = [(0, 1); (0, -1)]
let getOrtDirs =
    bimap abs abs >> function (1, 0) -> eastWest | (0, 1) -> southNorth

let dist (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

let rec walk target visited heats poss maze = 
    match Heap.tryUncons poss with 
    | Some ((_, pos, _), poss) when pos = target -> 
        match Map.tryFind (pos, southNorth) heats, Map.tryFind (pos, eastWest) heats with 
        | Some (heat1, path1), Some (heat2, path2) -> 
            if heat1 < heat2 then heat1, target::path1, maze 
            else heat2, target::path2, maze 
        | _ -> walk target visited heats poss maze
    | None -> 
        match Map.tryFind (target, southNorth) heats, Map.tryFind (target, eastWest) heats with 
        | Some (heat, path), _ | _, Some (heat, path) -> heat, target::path, maze 
        | _ -> -1, [], maze
    | Some ((_, pos, ortDirs), poss) 
        when Set.contains (pos, ortDirs) visited -> 
        // printfn "Visited: %A" pos
        walk target visited heats poss maze 
    | Some ((_, pos, ortDirs), poss) ->
        let accHeat, path = Map.findOrDefault (pos, ortDirs) (0, []) heats
        // printfn "%A %A %A %A" accHeat pos path ortDirs 
        let newPath = pos::path
        let visited = Set.add (pos, ortDirs) visited
        let heats, nextPoss = 
            ortDirs 
            |> Seq.fold(fun (heats, nextPoss) ortDir -> 
                let newOrtDirs = getOrtDirs ortDir
                [1..10] |> Seq.map (mulTuple ortDir)
                |> Seq.map ((++) pos)
                |> Seq.map(fanout id (flip (getMazeHeat (-1)) maze))
                |> Seq.filter(snd >> (<=) 0)
                |> Seq.mapFold(fun accHeat (pos, cellHeat) -> 
                    let accHeat = accHeat + cellHeat
                    (pos, accHeat), accHeat) accHeat
                |> fst 
                |> Seq.filter(fst >> dist pos >> (<=) 4)
                |> Seq.filter(fun (pos, accHeat) -> 
                    match Map.tryFind (pos, newOrtDirs) heats with 
                    | Some (prevHeat, _) when prevHeat <= accHeat -> false
                    | _ -> true)
                |> Seq.filter(fst >> flip tuple2 newOrtDirs >> flip Set.contains visited >> not)
                |> Seq.fold(fun (heats, nextPoss) (pos, accHeat) -> 
                    Map.add (pos, newOrtDirs) (accHeat, newPath) heats, (accHeat, pos, newOrtDirs)::nextPoss
                ) (heats, nextPoss)) (heats, [])    
        let poss = Heap.merge poss (Heap.ofSeq false nextPoss)
        // nextPoss |> Seq.iter(printfn "\t%A\n-------------------")
        // poss |> Seq.iter(printfn "\t\t%A")
        walk target visited heats poss maze

let part2 = 
    File.ReadAllLines
    >> Seq.map (Seq.map (string >> int) >> Seq.toArray) >> Seq.toArray
    >> fun maze -> 
            let target = (Seq.length maze - 1, (Seq.head >> Seq.length) maze - 1)
            walk target Set.empty Map.empty (Heap.ofSeq false [0, (0, 0), [(0, 1); (1, 0)]]) maze
    >> fun (accHeat, path, maze) -> 
        let pathS = 
            path 
            |> Seq.rev |> Seq.pairwise 
            |> Seq.map (fun ((i1, j1),(i2, j2)) -> 
                Seq.allPairs [min i1 i2..max i1 i2] [min j1 j2.. max j1 j2])
            |> Seq.concat |> set 
        let pathMaze = 
            maze |> Array.mapi(fun i -> 
                Array.mapi(fun j d -> 
                    if Set.contains (i, j) pathS then '#'
                    else (string d |> Seq.head))
                >> System.String)
            |> fun s -> System.String.Join('\n', s)
        // printfn "%s" pathMaze
        accHeat, path
            
part2 "./data/d17.txt"    