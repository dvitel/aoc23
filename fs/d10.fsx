open System 
open System.IO 
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let getPos defV (i, j) = 
    Seq.tryItem i >> Option.defaultValue Array.empty 
    >> Seq.tryItem j >> Option.defaultValue defV

let rec walk stepId (maze: char[][]) = 
    Seq.collect(fun (i, j) ->         
        let curCh = getPos '.' (i, j) maze
        let newPos = 
            match curCh with 
            | '-' -> [0,-1; 0,1]
            | '|' -> [-1,0; 1,0]
            | 'L' -> [-1,0; 0,1]
            | 'J' -> [0,-1; -1,0]
            | 'F' -> [1,0; 0,1]
            | '7' -> [1,0; 0,-1]
            | 'S' -> [-1,0; 1,0; 0,-1; 0,1]
            | _ -> []
            |> Seq.collect(fun (di, dj) ->
                let ij2 = (i + di, j + dj)
                // printfn "\t(%d, %d) %A %A" di dj ij2 (getPos '.' ij2 maze)
                match di, dj, getPos '.' ij2 maze with 
                | -1, 0, ('|'|'F'|'7') 
                | 1, 0, ('|'|'L'|'J')
                | 0, -1, ('-'|'L'|'F')
                | 0, 1,  ('-'|'J'|'7') -> [ij2]
                | _ -> []
            )
        maze.[i].[j] <- 
            match curCh with 
            | ('S'|'|') -> '1' | 'F' -> 'a' |'J' -> 'b' 
            | 'L' -> 'c' |'7' -> 'd' | '-' -> '2' | ch -> ch
        newPos)
    >> Seq.toList
    >> function 
        | [] -> stepId, maze
        | lst -> 
            // printfn "%d\t %A" stepId lst
            walk (stepId + 1) maze lst

let part1 = 
    File.ReadAllLines
    >> fork 
        (Seq.map Seq.toArray >> Seq.toArray) //build maze
        (Seq.indexed 
            >> Seq.pick (fun (i, s) ->
                s |> Seq.tryFindIndex ((=) 'S') 
                |> Option.map(fun j -> (i, j)))
            >> List.singleton) //find initial position of S
    ||>> walk 0

part1 "./data/d10.txt" |> fst
let part2 = 
    part1
    >> snd 
    >> Seq.mapi(fun i -> 
        Seq.fold(fun (cnt,ints, waitForOpt) ch -> 
            match waitForOpt, ch, ints % 2 = 1 with 
            | None, '1', _ -> (cnt, ints + 1, None)
            | None, 'a', _ -> (cnt, ints, Some 'b')
            | None, 'c', _ -> (cnt, ints, Some 'd')
            | Some 'b', 'd', _ | Some 'd', 'b', _ -> (cnt, ints, None)
            | Some 'b', 'b', _ | Some 'd', 'd', _ -> (cnt, ints + 1, None)
            | Some _, _, _ -> (cnt, ints, waitForOpt)
            | None, _, false -> cnt, ints, None 
            | None, _, true -> cnt + 1, ints, None
        ) (0, 0, None)
        >> fun (cnt, _, _) -> cnt)
    >> Seq.sum
    
part2 "./data/d10.txt"
    