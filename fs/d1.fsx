open System.IO
open System

let lines = 
    "data/d1.txt"
    |> File.ReadLines

let inline fork f1 f2 x = (f1 x, f2 x)
let inline droparg f x _ = f x 
let inline value x _ = x
let inline (||>>) f1 f2 x = f1 x ||> f2
let charToInt = fork id (value '0') ||>> (-) >> int

let part1: seq<string> -> int = 
    Seq.map(Seq.filter Char.IsDigit >> Seq.map charToInt >> fork (Seq.head >> (*) 10) Seq.last ||>> (+))
    >> Seq.sum

let inline (-->) cond f = fun x -> x |> if cond x then f >> Some else value None
let inline ``when`` conds x = Seq.pick(fun f -> f x) conds
let inline otherwise x = value true x
let inline revargs f x y = f y x

let map f s = Seq.mapi (fun i el -> f el i s) s

Seq.zip "one" "ones"

let part2: seq<string> -> int =
    Seq.map( 
        map(``when`` [ 
            Char.IsDigit --> (charToInt |> droparg |> droparg)
            (Seq.skip >> [] |> droparg)
            otherwise --> (value 0 |> droparg |> droparg)
            ])
        >> Seq.filter ((<)0)
        >> fork (Seq.head >> (*) 10) Seq.last
        ||>> (+))
    >> Seq.sum

part2 lines


