open System
open System.IO 
let inline forkSeq f1 f2 x = [f1 x; f2 x]
let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)

let mirrorSplits splitPoint= 
    Seq.mapi(fun lineNo sq ->
        Seq.initInfinite (fun i -> 
            // printfn "LineNo %d Split %d: %A" lineNo splitPoint [splitPoint - i; splitPoint + 1 + i]
            [splitPoint - i; splitPoint + 1 + i]
            |> Seq.map(fun k -> Seq.tryItem k sq)
            |> Seq.pairwise |> Seq.head
            ||> Option.map2 (fun a b -> if a = b then 0 else 1))
        |> Seq.takeWhile Option.isSome |> Seq.map Option.get
        |> Seq.scan (+) 0)    

let sol cond =
    File.ReadAllText
    >> split "\n\n"
    >> Seq.map(
        split "\n"
        >> forkSeq (Seq.map Seq.cast) Seq.transpose
        >> Seq.indexed
        >> Seq.tryPick(fun (i, m) ->
                Seq.init (Seq.length (Seq.head m) - 1) id
                |> Seq.tryFind(fun i -> mirrorSplits i m |> cond)
                |> Option.map(fun v -> (v + 1) * pown 100 i))
        >> Option.get)
    >> Seq.reduce (+)    
    
let part1 = sol (Seq.map (Seq.exists ((=) 1)) >> Seq.forall ((=) false)) 
let part2 = sol (fun splits -> 
                (Seq.forall (Seq.exists((=) 2) >> not) splits) && (Seq.filter(Seq.exists((=) 1)) splits |> Seq.length) = 1)

part1 "./data/d13.txt"    
part2 "./data/d13.txt"