open System 
open System.IO 

let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let inline uncurry f (x, y) = f x y

let rec getDiffs acc sq = 
    if Seq.forall ((=) 0L) sq then acc 
    else getDiffs (sq::acc) (sq |> (Seq.pairwise >> Seq.map(uncurry (-))))

let rec nextNum toAdd =
    function | [] -> toAdd | sq::otherSeqs -> nextNum (toAdd + Seq.head sq) otherSeqs

let part1 = 
    File.ReadAllLines
    >> Seq.map(split " " >> Seq.map int64)
    >> Seq.map(Seq.rev >> getDiffs [] >> nextNum 0L)
    >> Seq.sum

let part2 = 
    File.ReadAllLines
    >> Seq.map(split " " >> Seq.map int64)
    >> Seq.map(getDiffs [] >> nextNum 0L)
    >> Seq.sum

part2 "./data/d9.txt"