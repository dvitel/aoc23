open System 
open System.IO 

let inline fork f1 f2 x = (f1 x, f2 x)
let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let join (sep: string) (sq: seq<string>) = String.Join(sep, sq |> Seq.toArray)

let withMemo memo f data = 
    match Map.tryFind data memo with 
    | None -> let res, memo = f data in res, Map.add data res memo
    | Some res -> res, memo 

let rec takeHashes memo left n sizes =
    match List.skip n left with 
    | _ when Seq.take n left |> Seq.exists ((=) '.') -> 0L, memo
    | '#'::_ -> 0L, memo 
    | [] as tail | _::tail -> computePatterns memo (tail, sizes)        
and computePatterns memo =
    withMemo memo (function
        | left, [] when Seq.exists ((=) '#') left -> 0L, memo 
        | _, [] -> 1L, memo
        | left, sizes when Seq.length left < (Seq.sum sizes + Seq.length sizes - 1) -> 0L, memo
        | '.'::left, sizes -> computePatterns memo (left, sizes)
        | '#'::left, n::sizes -> takeHashes memo left (n-1) sizes
        | '?'::left, n::sizes ->
            let assumedHash, memo = takeHashes memo left (n-1) sizes
            let assumedDot, memo = computePatterns memo (left, n::sizes)
            assumedDot + assumedHash, memo
        | unknown -> failwithf "Unmatched %A" unknown)

let commonEnd = 
    Seq.mapFold computePatterns Map.empty
    >> fst >> Seq.sum 
let part1 = 
    File.ReadAllLines
    >> Seq.map(split " "
        >> fork (Seq.head >> Seq.toList) (Seq.last >> split "," >> Seq.map int >> Seq.toList))
    >>  commonEnd

part1 "./data/d12.txt"

let part2 = 
    File.ReadAllLines
    >> Seq.map(split " "
        >> fork 
            (Seq.head >> Seq.replicate 5 >> join "?" >> Seq.toList) 
            (Seq.last >> Seq.replicate 5 >> join "," >> split "," >> Seq.map int >> Seq.toList))
    >> commonEnd

part2 "./data/d12.txt"