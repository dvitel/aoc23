open System
open System.IO
#r "nuget: FSharpx.Collections"
#r "nuget: FSharpPlus"
open FSharpx.Collections
open FSharpPlus

// Functors - maps over 
[1..10] |> map ((*) 10)
seq {1..10} |> map ((*) 10)
[|1..10|] |> map ((*) 10)
Some 10 |> map ((+) 9)

let test = 
    File.ReadAllLinesAsync
    >> Async.AwaitTask
    >> map (Seq.map (Seq.map (string >> int)))
    >> Async.RunSynchronously

test "./data/d17.txt"

Char.ToUpper <!> "test"

let a:Async<_> = result 5

(*) 10 <!> (1,20)

lift2 tuple2 [-1..1] [-1..1]

// ((*) <<| [1..10]) <*> [0..1]

"test" |>> Char.ToUpper

zip (Some 5) (Some 10) |>> uncurry (*)

"test" |> map Char.ToUpper

// Applicative functors
Some (+) <*> Some 42
Some 42 |> map ((+) 10 >> string)
Some ((+) 10 >> string) <*> Some 42

Some (+) <*> None <*> (Some (+) <*> Some 2 <*> Some 2)

let getLine: Async<int> = result (Console.ReadLine >> int) <*> result ()

result (+) <*> getLine <*> getLine 
|> Async.RunSynchronously

Seq.initInfinite (konst 5)

join [[5];[1;2;3]]

[5..10] |>> fanout ((*) 10) ((+) 10)
[5..10] >>= fun i -> [1..i] 

[(+) 1] <*> [1;2;3]
[1;2;3] >>= fun i -> [(+) 1] >>= fun f -> [f i]
[1;2;3] >>= ((+) 1 >> result)

(+) <!> [5] <*> [5]



seq { (+); (*)} <*> seq{ 1; 2} <*> seq{3;4}

let s: seq<int> = result 5


let read = async { System.Console.ReadLine() |> tryParse<int> }
result (+) <*> result 2 <*> result 3
|> Async.RunSynchronously


let listFunc = function 0 -> 1 | 1 -> 2 | 2 -> 3 // [1;2;3]
let r01'' = map (fun x -> string (x + 10)) listFunc

// 5 |> ((+) 2 |> map ((*) 10))

// 42 |> toBytes




let get i j = 
    Seq.tryItem i >=> Seq.tryItem j

[(*);(+)] <*> [1;2;3] <*> [5]

get 1 3 [[1;2;3];[4;5;6]]

let get defV (i, j) maze = 
    Seq.tryItem i maze |> Option.bind (Seq.tryItem j) |> Option.defaultValue defV
    
let getMap defV key memo = 
    Map.tryFind key memo |> Option.defaultValue defV

["hello"; " "; "world"] >>= Seq.toList

// File.ReadAllLines >=> Seq.toList
// 5 |> ((fun i -> Seq.init i id) >=> (fun i -> Seq.init i id))

Seq.init 3 id 
|> intersperse 1
|> Seq.map (tap (printfn "%A"))
|> Seq.toList 
// |> map ((*) 10)

([1;2;3],3) |>> (*) 2

//  |> _1 .-> 0


// head None
// fanin ((+) 17) (List.length)

// 5 |> fanout ((*) 100) ((+) 42) 
// |> (-) 45 *** ((/)10)
// let inline fork f1 f2 x = (f1 x, f2 x)

(+) 1 <!> [1;2;3]

Some 42 <&> (+) 1


open Lens

view _1 ("test", "other")

"hello" ^. to' length
setl _2 42 ("hello", "world")


// let res = _Some ((*) 2) (Some 10)

[(+)] <*> [1;2;3]

// app [(+)] [1;2;3]

// split 1 [2;3;1;4;5]
String.split ["123"] "test123"
Seq.split [[1]] [-10..10]


contramap (fun (x: int) -> string x) (printfn "%s")

(13,2)
|> bimap ((*) 2) ((+) 14)

(1,2 )
|> first ((*) 2)

// tuple - is bifunctor and comoned

[1;2;3] >>= ((+) 1 >> result)

(1,2) |> extract

(17,19) =>> konst 42

1 +  2
// seq { 1;2;3; } ++ seq{ 4;5}

Map [1, "test"; 2, "other"] ++ Map [2,"other2";3, "in"]

set [1..5] ++ set [3..7]

let f1 (x:int) = string x 
let f2 (x: string list) = List.head x

f1 ++ f2

sum [[1..5];[7..10]]

Seq.sum [Some 42; None; Some 17]


[1..3] <|> [1..5]

(1,2)

traverse (fun x -> [1..x]) [1..3]


let a = (+) 1 >> string
let a: int -> int = arr (fun (x:int) -> x + 1)

let b: int*string -> int*string = first a

b (5, "test")


(5, "test")
|> ((+) 1) *** (String.replicate 5)

(5, "test")
|> arrFirst ((+) 10)

(5, "test")
|> first ((+) 5)

5
|> fanout ((+) 15) ((*) 100)

// let t1: string -> int option = tryParse >=> ((+) 5 >> result)
Choice2Of2 "s"
|> fanin (tryParse >=> ((+) 5 >> result)) ((*) 100 >> Some)

((*) 5, 10)
|> app

// |> second (arr (fun (x:string) -> x + "END"))  

// (+) 1 >>> (fun x -> string x)

(*) 5 <!> [1..5]

plus (1,2) (3,4)

async.Return 5 ++ async.Return 10

let s = put (1,1)


