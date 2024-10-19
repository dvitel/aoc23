open System.IO

let inline fork f1 f2 x = (f1 x, f2 x)
let inline forkList fs x = List.map(fun f -> f x) fs
let getOrDef key def = Map.tryFind key >> Option.defaultValue def
let split (sep: string) (x:string) = x.Split(sep)
let inline tupleRev (x, y) = (y, x)
let inline uncurry f (x, y) = f x y
let apply f x y = Seq.zip x y |> Seq.map(uncurry f)

let part1 =     
    File.ReadAllLines 
    >> Seq.map (split ": ")
    >> Seq.filter (
        Seq.last
        >> split "; "
        >> Seq.forall
            (split ", "
            >> Seq.map(
                split " "
                >> fork (Seq.head >> int) Seq.last
                >> tupleRev)
            >> Map
            >> forkList [getOrDef "red" 0; getOrDef "green" 0; getOrDef "blue" 0]
            >> apply (>=) [12;13;14]
            >> Seq.forall id
            ))
    >> Seq.map (
        Seq.head
        >> split " "
        >> Seq.last 
        >> int)
    >> Seq.sum

let part2 =     
    File.ReadAllLines 
    >> Seq.map (split ": ")
    >> Seq.map (
        Seq.last
        >> split "; "
        >> Seq.map
            (split ", "
            >> Seq.map(
                split " "
                >> fork (Seq.head >> int) Seq.last
                >> tupleRev)
            >> Map
            >> forkList [getOrDef "red" 0; getOrDef "green" 0; getOrDef "blue" 0]            
            )
        >> Seq.cast<seq<int>>
        >> Seq.reduce (apply max)
        >> Seq.fold (*) 1
        )
    >> Seq.sum

part2 "./data/d2.txt"
