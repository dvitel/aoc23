open System 
open System.IO 

let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let rec uniqPairs acc = 
    function
    | [] -> acc 
    | x::other -> 
        let acc = List.fold(fun acc y -> (x, y)::acc) acc other
        uniqPairs acc other

let part1 = 
    File.ReadAllLines
    >> fork 
        (fork 
            (Seq.indexed >> Seq.filter(snd >> Seq.forall((=) '.')) >> Seq.map fst >> Seq.toList)
            (Seq.transpose >> Seq.indexed >> Seq.filter(snd >> Seq.forall((=) '.')) >> Seq.map fst >> Seq.toList)
        )    
        (Seq.indexed 
         >> Seq.collect 
            (fork fst
                (snd >> Seq.indexed >> Seq.filter(snd >> (=) '#') >> Seq.map fst)
             >> fun (i, js) -> Seq.map(fun j -> (i, j)) js)
         >> Seq.toList >> uniqPairs [])
    ||>> fun (expandX, expandY) -> 
        Seq.map(fun ((x1, y1), (x2, y2)) -> 
            let xMin, xMax, yMin, yMax = min x1 x2, max x1 x2, min y1 y2, max y1 y2
            int64(xMax - xMin + yMax - yMin) + 
                999999L * 
                    ((expandX |> Seq.filter (fun x -> x > xMin && x < xMax) |> Seq.length |> int64) + 
                     (expandY |> Seq.filter (fun y -> y > yMin && y < yMax) |> Seq.length |> int64))
        )
        >> Seq.sum

part1 "./data/d11.txt"

