open System
open System.IO 
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2
let split (sep: string) (x:string) = x.Split(sep)
let join (sep: string) (sq: seq<string>) = String.Join(sep, sq)

// let rec tiltRow dir prevId curId oCount counts = 
//     function 
//     | '#'::other when oCount > 0 -> 
//         tiltRow dir curId (curId + 1) 0 ((dir (prevId, curId), oCount)::counts) other 
//     | '#'::other -> tiltRow dir curId (curId + 1) 0 counts other
//     | 'O'::other -> tiltRow dir  prevId (curId + 1) (oCount + 1) counts other 
//     | '.'::other -> tiltRow dir  prevId (curId + 1) oCount counts other 
//     | [] when oCount > 0 -> (dir (prevId, curId),oCount)::counts
//     | [] -> counts

let tilt sort = Seq.map(split "#" >> Seq.map sort >> join "#")

// let tilt buildPos dir = 
//     Seq.map Seq.toList
//     >> Seq.map (tiltRow dir -1 0 0 [])
//     >> Seq.mapi (fun i -> Seq.map(fun (j, count) -> (buildPos i j, count)))
//     >> Seq.concat    

// let mutable m: char[][] = null
// let mutable os: (int * int) list = []
// let setState v = 
//     m <- v 
//     os <- 
//         m |> Seq.mapi(fun i -> 
//             Seq.mapi(fun j ch -> ((i, j), ch)) 
//             >> Seq.filter (snd >> (=) 'O') 
//             >> Seq.map fst) 
//           |> Seq.concat |> Seq.sort |> Seq.toList
//     v

// let updateState pickI pickJ s =     
//     os |> Seq.iter (fun (i, j) -> m.[i].[j] <- '.')
//     os <-
//         (s |> Seq.map(fun ((i, j), count) -> 
//             Seq.init count (fun k -> (pickI i k, pickJ j k)))
//           |> Seq.concat |> Seq.sort |> Seq.toList)    
//     os |> Seq.iter(fun (i, j) -> m.[i].[j] <- 'O')
//     m

let seqToStr = Seq.toArray >> String
let tilts = [
   Seq.transpose >> Seq.map seqToStr, Seq.sortDescending >> seqToStr
   id, Seq.sortDescending >> seqToStr
   Seq.transpose >> Seq.map seqToStr, Seq.sort >> seqToStr
   id, Seq.sort >> seqToStr
]

let score =
    Seq.map (Seq.rev >> Seq.indexed 
             >> Seq.filter(snd >> (=) 'O') >> Seq.map(fst >> (+) 1))
    >> Seq.concat >> Seq.sum

let move maxSteps (m: string list) =
    Seq.init maxSteps id 
    |> Seq.scan(fun (prevV, (cache, m: string list)) curStep  -> 
        match Map.tryFind m cache with 
        | Some prevStep -> ((maxSteps - curStep - 1) % (curStep - prevStep), (Map.empty, m))
        | None -> 
            let newM = 
                tilts |> Seq.fold(fun acc (orient, sort) -> 
                    acc |> orient |> tilt sort |> Seq.toList) m
            (-1, (Map.add m curStep cache, newM))
        ) (-1, (Map.empty, m))
let moves maxSteps = 
    move maxSteps
    >> Seq.find (fst >> (<) (-1))
    >> fork fst (snd >> snd)
    ||>> move
    >> Seq.last >> snd >> snd

let sol numMoves = 
    File.ReadAllLines 
    >> Seq.toList   
    >> moves numMoves
    // >> score 

sol 1000000000 "./data/d14.txt"
