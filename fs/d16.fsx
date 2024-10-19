open System.IO 

let get defVal (i, j) maze = 
    Seq.tryItem i maze |> Option.bind(Seq.tryItem j) 
    |> Option.defaultValue defVal

let nextPos (i, j) k = function 
    | '>' -> i, j + k + 1 | '<' -> i, j - k - 1
    | '^' -> i - k - 1, j | 'v' -> i + k + 1, j

let search dir pos maze = 
    Seq.initInfinite (fun k -> 
        let pos = nextPos pos k dir
        pos, get '#' pos maze)
    |> Seq.find (snd >> (<>) '.')

let beamed (i1, j1) (i2, j2) symb = 
    Seq.allPairs [min i1 i2..max i1 i2] [min j1 j2..max j1 j2]
    |> set |> Set.remove (i1, j1) |> if symb = '#' then Set.remove (i2, j2) else id

let rec move visited  dirPoss maze = 
    match dirPoss with 
    | [] -> visited
    | dirPos::other when Map.containsKey dirPos visited -> move visited other maze
    | (dir, pos)::other ->         
        let newPos, symb = search dir pos maze
        let beamedSet = beamed pos newPos symb
        let newDirPoss = 
            match dir, symb with 
            | _, '#' -> []
            | '>', '\\' -> ['v', newPos]
            | '>', '/' -> ['^', newPos]
            | '<', '\\' -> ['^', newPos]
            | '<', '/' -> ['v', newPos]
            | 'v', '\\' -> ['>', newPos]
            | 'v', '/' -> ['<', newPos]
            | '^', '\\' -> ['<', newPos]
            | '^', '/' -> ['>', newPos]
            | ('>'|'<'), '|' -> [('^', newPos); ('v', newPos)]
            | ('^'|'v'), '-' -> [('>', newPos); ('<', newPos)]
            | _ -> [dir, newPos]
        let newVisited = Map.add (dir, pos) (beamedSet, newDirPoss) visited
        move newVisited (newDirPoss @ other) maze 

let rec gatherBeamed acc poss visited = 
    match poss with 
    | [] -> acc 
    | p::other -> 
        let beamedSet, next = Map.find p visited
        let newVisited = Map.remove p visited
        let poss = 
            Seq.concat [next; other] |> Seq.filter (fun x -> Map.containsKey x newVisited) 
            |> Seq.toList
        gatherBeamed (beamedSet::acc) poss newVisited

let part1 = 
    File.ReadAllLines
    >> move Map.empty [('>', (0, -1))]
    >> gatherBeamed [] [('>', (0, -1))]
    >> Set.unionMany
    >> Set.count

part1 "./data/d16.txt"

let part2 = 
    File.ReadAllLines
    >> fun maze -> 
        let dirs = 
            Seq.concat [
                Seq.init maze.Length (fun i -> '>', (i, -1))
                Seq.init maze.Length (fun i -> '<', (i, maze[0].Length))
                Seq.init maze[0].Length (fun i -> 'v', (-1, i))
                Seq.init maze[0].Length (fun i -> '^', (maze.Length, i))
            ] |> Seq.toList
        let visited = move Map.empty dirs maze
        dirs |> Seq.map(fun dir ->
            gatherBeamed [] [dir] visited
            |> Set.unionMany |> Set.count)
        |> Seq.max
    
part2 "./data/d16.txt"