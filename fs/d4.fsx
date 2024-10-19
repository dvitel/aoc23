open System
open System.IO

let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let tuple x = (Seq.head x, Seq.last x)

let rec count acc = 
    function 
    | g::gs,n::ns when g < n -> count acc (gs, n::ns)
    | g::gs,n::ns when g > n -> count acc (g::gs, ns)
    | g::gs,n::ns when g = n -> count (acc + 1) (gs, ns)
    | _ -> acc 

let common = 
    File.ReadAllLines 
    >> Seq.map(split ": " >> Seq.last >> split " | "
        >> Seq.map(split " " >> Seq.map int >> Seq.sort >> Seq.toList)
        >> tuple
        >> count 0)
let part1 = 
    common
    >> Seq.filter ((<) 0)
    >> Seq.map (fun x -> 2. ** float(x - 1))
    >> Seq.sum

let getNumCards acc i = Map.tryFind i acc |> Option.defaultValue 1
let setNumCards num acc i = Map.add i (getNumCards acc i + num) acc
let rec count2 lim acc i = 
    function 
    | [] -> acc 
    | wins::cards -> 
        let num = getNumCards acc i
        let acc = 
            Seq.init wins ((+) (i+1))
            |> Seq.filter ((>) lim)
            |> Seq.fold(setNumCards num) acc
        count2 lim acc (i+1) cards
let part2 = 
    common
    >> Seq.toList
    >> fun cards -> 
        count2 (Seq.length cards) Map.empty 0 cards
        |> Map.toSeq |> Seq.map (snd >> (+) (-1)) |> Seq.sum 
        |> (+) (Seq.length cards)

part2 "./data/d4.txt"