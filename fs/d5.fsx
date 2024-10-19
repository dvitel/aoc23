open System
open System.IO
open System.Text.RegularExpressions
let r = Regex("""(?<from>.*)-to-(?<to>.*)\smap:\n(?<lines>(?:.|\s)*?)\n\n""")
let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let rec mapper acc (xs, xe) = 
    function 
    | [d; s; l]::mappings -> 
        let remap xi = d + (xi - s)
        let e = s + l
        if xe <= s then (xs, xe)::acc
        else if e <= xs then mapper acc (xs, xe) mappings
        else if xe <= e && xs < s then 
            (xs, s)::(remap s, remap xe)::acc
        else if xe <= e && xs >= s then 
            (remap xs, remap xe)::acc
        else if xe >= e && xs >= s then 
            mapper ((remap xs, remap e)::acc) (e, xe) mappings
        else mapper ((xs, s)::(remap s, remap e)::acc) (e, xe) mappings
    | _ -> (xs, xe)::acc

let rec mapAll (source, values) mappers = 
    match Map.tryFind source mappers with 
    | None -> source, values 
    | Some (dest, mapper) -> 
        let mp = values |> Seq.collect mapper
        mapAll (dest, mp) mappers

let part1 x = x |> Seq.map (fun x -> (x, x + 1L))
let part2 (x: int64 seq) = x |> (Seq.chunkBySize 2 >> Seq.map (fork Seq.head (fork Seq.head Seq.last ||>> (+))))
let sol part =
    File.ReadAllText
    >> fork 
        (split "\n"
        >> Seq.head >> split ": " >> Seq.last 
        >> split " " >> Seq.map int64
        >> fun seeds -> ("seed", part seeds))
        (r.Matches
        >> Seq.map(fun m -> 
            (m.Groups.["from"].Value, (m.Groups.["to"].Value, 
                m.Groups.["lines"].Value
                |> split "\n"
                |> Seq.map (split " " >> Seq.map int64 >> Seq.toList)
                |> Seq.sortBy (Seq.skip 1 >> Seq.head)
                |> Seq.toList
                |> fun mappings x -> mapper [] x mappings)))
        >> Map)
    ||>> mapAll
    >> snd >> Seq.min >> fst

sol part2 "./data/d5.txt"