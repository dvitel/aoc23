open System.IO
#r "nuget: FSharpPlus"
open FSharpPlus

let delta n = function "R" -> (0L, n) | "L" -> (0L, -n) | "U" -> (-n, 0L) | "D" -> (n, 0L)

let rec foldWhile pred f acc = 
    function 
    | [] -> acc, []
    | x::other when pred x -> foldWhile pred f (f acc x) other 
    | other -> acc, other

let rec collectAreas area (depth, spans) = 
    function
    | [] -> area     
    | (nextDepth, nextSpans)::other -> 
        let d = nextDepth - depth + 1L
        let newArea = spans |> Seq.map (fanout snd fst >> uncurry (-) >> (+) 1L >> (*) d) |> Seq.sum        
        let collected, newSpans = 
            spans |> Seq.fold(fun (collected, nextSpans) span -> 
                let collected, nextSpans = 
                    foldWhile (snd >> ((>=) (fst span))) (flip List.cons) collected nextSpans
                let (leftSpan, collected), nextSpans = 
                    foldWhile (snd >> ((>=) (snd span))) (fun (span, collected) nextSpan -> 
                        let leftSpan, beforeSpan = 
                            span 
                            |> fanout (first (snd nextSpan |> konst)) (second (fst nextSpan |> konst))
                        leftSpan, beforeSpan::collected) (span, collected) nextSpans
                leftSpan::collected, nextSpans) ([], nextSpans)
        let newSpans = 
            collected |> Seq.filter (fanout snd fst >> uncurry (>)) 
            |> Seq.fold(flip List.cons) newSpans
            |> List.fold(curry (function 
                               | (startS, x)::other, (y, endS) when x = y -> (startS, endS)::other
                               | acc, x -> x::acc)) []
            |> List.rev
        let rec getOverlap acc =
            function 
            | [], _ | _, [] -> acc 
            | (x1, y1)::other1, (x2, y2)::other2 when y1 < x2 -> getOverlap acc (other1, (x2, y2)::other2)
            | (x1, y1)::other1, (x2, y2)::other2 when y2 < x1 -> getOverlap acc ((x1, y1)::other1, other2)
            | (x1, y1)::other1, (x2, y2)::other2 ->                 
                let overlap = min y1 y2 - max x1 x2 + 1L
                if y1 >= y2 then getOverlap (acc + overlap) ((x1, y1)::other1, other2)
                else getOverlap (acc + overlap) (other1, (x2, y2)::other2)
        let overlap = getOverlap 0L (spans, newSpans)
        printfn "> Area %d. %A. Depth %d. Diff %d. Ov: %d. %A" newArea spans depth d overlap nextSpans
        collectAreas (area + newArea - overlap) (nextDepth, newSpans) other

let common = 
    Seq.scan(fun acc (ch, x) -> acc ++ delta x ch) (0L, 0L)
    >> Seq.pairwise
    >> Seq.filter (bimap fst fst >> uncurry (=))
    >> Seq.map (fanout (fst >> fst) (bimap snd snd))
    >> Seq.map (bimap id (fanout (uncurry min) (uncurry max)))
    >> Seq.sort >> Seq.groupBy fst
    >> Seq.map (second (Seq.map snd >> Seq.toList))
    >> fanout (Seq.head >> fst >> (flip (-)) >> first) id 
    >> uncurry Seq.map
    >> Seq.toList
    >> fanout List.head List.tail
    >> uncurry (collectAreas 0L)  

let part1 = 
    File.ReadAllLines
    >> Seq.map (split [" "] >> Seq.take 2
        >> fanout Seq.head (Seq.last >> int64))
    >> common

part1 "./data/d18.txt"

let part2 = 
    File.ReadAllLines
    >> Seq.map (split [" "] >> Seq.last
        >> fanout (skip 7 >> take 1 >> function "0" -> "R" | "1" -> "D" | "2" -> "L" | "3" -> "U") 
            (skip 2 >> take 5 >> fun x -> System.Convert.ToInt64(x, 16)))
    >> common
part2 "./data/d18.txt"
