open System 
open System.IO 

let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let opt (t0, d0) = 
    // (t0 - t) * t > d0 ==> 0 > t * t - t0 * t + d0 ==> t \in [ceil(t1), floor(t2)] t1,2 = (t0 -+ sqrt(t0^2 - 4 * d0)) / 2
    (ceil >> int) ((t0 + sqrt(t0 * t0 - 4. * d0)) / 2.) - (floor >> int) ((t0 - sqrt(t0 * t0 - 4. * d0)) / 2.) - 1

let part1 = split " " >> Seq.map float
let part2 s = s |> (Seq.filter ((<>) ' ') >> Seq.toArray >> String >> float >> Seq.singleton)

let sol part = 
    File.ReadAllLines
    >> Seq.map (split ":" >> Seq.last >> part)
    >> fork Seq.head Seq.last 
    ||>> Seq.zip
    >> Seq.map opt
    >> Seq.reduce (*)


sol part2 "./data/d6.txt"