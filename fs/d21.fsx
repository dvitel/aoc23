open System.IO
#r "nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions

let get defVal (i, j) = 
    Seq.tryItem i >=> Seq.tryItem j >> Option.defaultValue defVal

let get2 (i, j) maze = 
    let iL, jL = fanout Seq.length (Seq.head >> Seq.length) maze
    let wrap xL x = if x < 0 then x + xL else x
    let i, j = (i % iL, j % jL) |> bimap (wrap iL) (wrap jL)
    (Seq.item i >> Seq.item j) maze 

let flatIndexed = Seq.mapi(fun i -> Seq.mapi(fun j -> tuple2 (i, j))) >> join

// let steps dir = 
//     let ort = (fanout (snd >> abs) (fst >> abs)) dir 
//     [ dir; ort; bimap negate negate ort]

let steps = [(-1, 0); (1, 0); (0, -1); (0, 1)]

let rec walk maxWalk maze i (evenSets, oddSets) = 
    if i = maxWalk then  (evenSets, oddSets) 
    else 
        let curSet, prevSet =
            if i % 2 = 0 then evenSets, oddSets
            else oddSets, evenSets
            |> bimap List.head List.head
        let newCells = 
            curSet |> Seq.collect(fun c -> steps |> Seq.map(fun s -> c ++ s))
            |> Seq.filter(flip Set.contains prevSet >> not)
            |> Seq.filter(fun c -> get2 c maze <> '#')
            |> set
        // printfn "%A" newCells
        let newSets = if i % 2 = 0 then evenSets, (newCells::oddSets) else (newCells::evenSets), oddSets
        walk maxWalk maze (i+1) newSets

let part1 steps = 
    File.ReadAllLines
    >> Seq.map(Seq.toArray) >> Seq.toArray
    >> fanout (flatIndexed >> Seq.find (snd >> (=) 'S')  >> fst) id
    >> fun (start, maze) -> 
        walk steps maze 0 ([Set.singleton start], [Set.empty])

        // |> fst |> Seq.map(Seq.length) |> Seq.sum

let f, s = part1 1000 "./data/d21.txt"
let f1 = f |> Seq.rev |> Seq.map(Seq.length)
let s1 = s |> Seq.rev |> skip 1 |> Seq.map(Seq.length)
let rec merge s1 s2 = seq {
    match Seq.tryHead s1, Seq.tryHead s2 with 
    | Some x, Some y -> 
        yield x
        yield y
        yield! merge (Seq.tail s1) (Seq.tail s2)
    | _ -> ()
}

let res = merge f1 s1 |> Seq.toList

//Seq.length res
res |> Seq.mapi(fun i ith ->     
    let sm = 
        Seq.initInfinite (fun j -> i + j + 1) 
        |> Seq.takeWhile (fun j -> j < Seq.length res)
        |> Seq.map(fun j -> j, Seq.item j res)
        |> Seq.filter(fun (j, jth) -> jth = ith)
        // |> Seq.map fst 
        |> Seq.toList
    i, sm)
|> Seq.filter (snd >> Seq.length >> (<) 1) |> Seq.toList

// Seq.initInfinite (fun i ->     
// ) |> Seq.take 100 |> Seq.toList

let part2 = 
    File.ReadAllLines
    >> Seq.map(Seq.toArray) >> Seq.toArray
    >> fanout (flatIndexed >> Seq.find (snd >> (=) 'S')  >> fst) id
    >> fun (start, maze) -> 
        walk 1000 maze 0 ([Set.singleton start], [Set.empty])
        |> fst |> Seq.map(Seq.length) |> Seq.sum
    
part2 "./data/d21.txt" 