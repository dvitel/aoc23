open System 
open System.IO
let next grid (_, i, j) = 
    if j + 1 < (Seq.head >> Seq.length) grid then 
        (Seq.item i >> Seq.item (j + 1) >> Some) grid 
    else if i + 1 < Seq.length grid then 
        (Seq.item (i + 1) >> Seq.item 0 >> Some) grid 
    else None 
let near grid (_, i, j) = 
    seq { for a in [-1; 0; 1] do for b in [-1; 0; 1] -> a + i, b + j }
    |> Seq.filter(fun (i, j) -> i >= 0 && i < Seq.length grid && j >= 0 && j < (Seq.item 0 >> Seq.length) grid)
    |> Seq.map (fun (i, j) -> (Seq.item i >> Seq.item j) grid) 
let rec number grid (s, i, j) =
    seq {
        if Char.IsDigit s then 
            yield (s, i, j)
            match next grid (s, i, j) with 
            | Some y -> yield! number grid y 
            | _ -> ()
    }

let rec numbers grid = 
    function 
    | Some nextCell ->
        match number grid nextCell |> Seq.toList with 
        | [] -> next grid nextCell |> numbers grid
        | n ->
            let nearN = 
                Seq.collect (near grid >> Seq.filter (fun (s, _, _) -> not (Char.IsDigit s) && s <> '.'))
                >> Seq.toList
            match nearN n with 
            | [] -> numbers grid (n |> Seq.last |> next grid)
            | nearN -> 
                // printfn "Num %A: %A" n nearN
                seq {
                    yield (n, Set.ofSeq nearN)
                    yield! numbers grid (n |> Seq.last |> next grid)
                }
    | None -> Seq.empty

let common =
    File.ReadAllLines 
    >> Seq.mapi(fun i -> Seq.mapi(fun j ch -> (ch, i, j)) >> Array.ofSeq)
    >> Array.ofSeq
    >> fun grid -> numbers grid ((Seq.head >> Seq.head >> Some) grid)

let numToInt = Seq.map(fun (s, _, _) -> s) >> Array.ofSeq >> String >> int

let part1 = 
    common
    >> Seq.map(fst >> numToInt)
    >> Seq.sum

let part2 = 
    common 
    >> Seq.collect(fun (n, symbs) -> 
        symbs
        |> Seq.filter (fun (s, _, _) -> s = '*')
        |> Seq.map (fun s -> (s, n))) 
    >> Seq.groupBy(fst)
    >> Seq.filter(snd >> Seq.length >> (=) 2)
    >> Seq.map(snd >> Seq.map (snd >> numToInt) >> fun nms -> Seq.head nms * Seq.last nms)
    >> Seq.sum

part2 "./data/d3.txt"
    


// let inline uncurry f (x, y) = f x y
// let apply f a = Seq.zip a >> Seq.map(uncurry f)
// let applySeq fs x = Seq.map(fun f -> f x) fs
// let applyMany f = Seq.map(apply f) >> applySeq
// let nb_delta =  seq { for i in [-1; 1] do for j in [-1; 1] -> [i; j] }
// let inGrid grid i j = 
//     applyMany (+) nb_delta [i; j]
//     |> Seq.filter(
//         Seq.for
//     )

// let part1 =     
//     File.ReadAllLines 
//     |> Seq.map

// part1 "./data/d3.txt"
