open System 
open System.IO 
let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let strength1: string -> int = 
    Seq.groupBy id >> Seq.map(snd >> Seq.length) >> Seq.sortDescending >> Seq.toList
    >> function | 5::_ -> -1  | 4::_ -> -2    | 3::2::_ -> -3 
                | 3::_ -> -4  | 2::2::_ -> -5 | 2::_ -> -6    | _ -> -7
let strength2: string -> _ =     
    Seq.map(fun ch -> 
        ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
        |> Seq.findIndex((=) ch) |> (-) 0)

let sol strength1 strength2 = 
    File.ReadAllLines
    >> Seq.map (
        split " " 
        >> fork Seq.head (Seq.last >> int64) 
        >> fork id (fst >> 
            fork (strength1 >> Seq.singleton) strength2
            ||>> Seq.append
            >> Seq.toList))
    >> Seq.sortBy snd
    >> Seq.mapi(fun i -> fst >> snd >> (*) (int64 (i + 1)))
    >> Seq.sum

sol strength1 strength2 "./data/d7.txt"

let strength3: string -> int = 
    fork 
        (Seq.filter ((=) 'J') >> Seq.length)
        (Seq.filter ((<>) 'J') >> 
            Seq.groupBy id >> Seq.map(snd >> Seq.length) >> Seq.sortDescending >> Seq.toList)
    >> function
        | 5, _ -> -1 | jc, n::_ when jc = (5 - n) -> -1 
        | jc, n::_ when jc = (4 - n) -> -2
        | jc, n1::n2::_ when jc = (3 - n1) + (2 - n2) -> -3
        | jc, n1::_ when jc = (3 - n1) -> -4
        | jc, n1::n2::_ when jc = (2 - n1) + (2 - n2) -> -5
        | jc, n1::_ when jc = (2 - n1) -> -6
        | _ -> -7

let strength4: string -> _ =     
    Seq.map(fun ch -> 
        ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
        |> Seq.findIndex((=) ch) |> (-) 0)

sol strength3 strength4 "./data/d7.txt"