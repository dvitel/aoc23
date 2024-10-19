open System
open System.IO 
let split (sep: string) (x:string) = x.Split(sep)
let splitMany (seps: char[]) (x:string) = x.Split(seps, StringSplitOptions.RemoveEmptyEntries)

let hash = Seq.fold(fun acc ch -> (acc + int ch) * 17 % 256) 0 
let part1 = 
    File.ReadAllText
    >> split ","
    >> Seq.map hash
    >> Seq.sum

part1 "./data/d15.txt"    

let updateBox label boxes f = 
    let boxId = hash label
    let newBox = f ((Map.tryFind boxId >> Option.defaultValue Map.empty) boxes)
    Map.add boxId newBox boxes

let part2 = 
    File.ReadAllText
    >> split ","
    >> Seq.map(splitMany [| '-'; '=' |] >> Seq.toList)
    >> Seq.indexed
    >> Seq.fold(fun boxes -> 
        function 
        | i, label::[] -> updateBox label boxes (Map.remove label)
        | i, label::focalLength::[] -> 
            updateBox label boxes (fun box ->
                match Map.tryFind label box with 
                | None -> Map.add label (i, focalLength) box 
                | Some (j, _) -> Map.add label (j, focalLength) box)) Map.empty
    >> Map.toSeq
    >> Seq.collect(fun (boxId, lenses) -> 
        lenses |> Map.values |> Seq.sortBy fst |> Seq.map (snd >> int)
        |> Seq.indexed |> Seq.map(fun (i, focalLength) -> (boxId + 1) * (i + 1) * focalLength))
    >> Seq.sum

part2 "./data/d15.txt" 