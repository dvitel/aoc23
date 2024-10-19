open System.IO
#r "nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions
let r = Regex("(?<prop>[xmas])(?<op>[><])(?<val>\\d+)")

let parseWorkflow minVal maxVal = 
    split ["{"; "}"]
    >> fanout Seq.head 
        (skip 1 >> head
         >> split [","]
         >> Seq.rev
         >> fanout head 
                (Seq.tail >> Seq.rev 
                    >> Seq.map(split [":"] >> 
                        fanout Seq.last (Seq.head >> r.Match >> 
                            fun m -> 
                                let interval v = match  m.Groups.["op"].Value with | "<" -> (minVal, v) | _ -> (v, maxVal)
                                m.Groups.["prop"].Value, [interval (float(m.Groups.["val"].Value))]
                            >> Seq.singleton >> Map.ofSeq))))

let parsePart =
    split ["{"; "}"]
    >> skip 1 >> head 
    >> split [","] 
    >> Seq.map(split ["="] >> fanout head (Seq.last >> float))
    >> Map.ofSeq

let rec intersectI acc =
    function 
    | [], _ | _, [] -> acc |> Seq.filter (uncurry (flip (-)) >> (<) 1.) |> Seq.rev |> Seq.toList
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) -> 
        if y1 < y2 then other1, l2 else l1, other2
        |> intersectI ((max x1 x2, min y1 y2)::acc) 

let rec diffI acc: list<float*float>*list<float*float> -> list<float*float> =
    function 
    | [], _ -> acc |> Seq.filter (uncurry (flip (-)) >> (<) 1.) |> Seq.rev |> Seq.toList
    | l, [] -> acc |> List.fold(flip List.cons) l |> Seq.filter (uncurry (flip (-)) >> (<) 1.) |> Seq.toList
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when y1 <= x2 -> 
        diffI ((x1, y1)::acc) (other1, l2)   
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when y2 <= x1 -> 
        diffI acc (l1, other2)                    
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when x1 < x2 && y1 <= y2 -> 
        diffI ((x1, x2 + 1.)::acc) (other1, l2)
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when x1 < x2 && y2 < y1 -> 
        diffI ((x1, x2 + 1.)::acc) ((y2 - 1., y1)::other1, other2)                 
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when x2 <= x1 && y1 <= y2 -> 
        diffI acc (other1, l2)                 
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) when x2 <= x1 && y2 < y1 -> 
        diffI acc ((y2 - 1., y1)::other1, other2)                
    | a, b -> failwithf "Unfinished %A, %A" a b

diffI [] ([0., 100.; 100., infinity], [0., 100.])

let rec unionI acc = 
    function
    | [], l | l, [] -> acc |> Seq.fold(flip List.cons) l
    | (x1, y1)::other1 as l1, ((x2, y2)::other2 as l2) -> 
        if y1 <= x2 then 
            unionI ((x1, y1)::acc) (other1, l2)
        else if y2 <= x1 then 
            unionI ((x2, y2)::acc) (l1, other2) 
        else 
            let common = (min x1 x2, max y1 y2)
            if y1 < y2 then other1, common::other2 
            else common::other1, other2
            |> unionI acc

unionI [] ([(-infinity, 5.)], [(4, infinity)])

let mapVal defVal key = Map.tryFind key >> Option.defaultValue defVal
let onMaps defVal operation maps = 
    maps 
    |> (bimap (Map.keys >> set) (Map.keys >> set) >> uncurry Set.union)
    |> Seq.map(fanout id (mapVal defVal >> fun keyF -> maps |> bimap keyF keyF |> operation)) 
    |> Map.ofSeq

let negate defV intervals =
    diffI [] (defV, intervals)

negate [0., 4001.] [100., 1000.]

let rec comb acc = 
    function 
    | [] -> acc 
    | els::l -> comb (els |> List.collect(fun el -> acc |> List.map(fun accl -> el::accl))) l

// comb [[]] [[true;false];[true;false]]

let hasNoEmptyMap = Map.toSeq >> Seq.map snd >> Seq.forall (Seq.isEmpty >> not)

let onMapsComb defVal (m1, m2) =
    let keys = m2 |> Map.keys |> Seq.toList
    let diffM = 
        keys 
        |> Seq.map(fanout id (mapVal defVal >> fun keyF -> diffI [] (keyF m1, keyF m2))) 
        // |> Seq.filter(fun (key, diffs) -> mapVal defVal key m1 <> diffs)
        |> Seq.toList
    let keyCombs = 
        comb [[]] (List.replicate (Seq.length diffM) [true; false]) 
        |> set |> Set.remove (List.replicate (Seq.length diffM) false)
    keyCombs
    |> Seq.map(fun comb -> 
        Seq.zip diffM comb 
        |> Seq.fold(fun acc ((k, intervals), shouldBeSame) -> 
            let intervals = if shouldBeSame then intervals else negate (mapVal defVal k m1) intervals 
            Map.add k intervals acc) m1
        )
    |> Seq.filter hasNoEmptyMap
    |> Seq.toList
    // keys |> Seq.map(fun k -> Map.add k (Map.find k m) (fst maps))

// onMapsComb [0., 4001.] (Map [("a", [(0.0, 2006.0)]); ("s", [(0.0, 1351.0)]); ("x", [(2662.0, 4001.0)])], 
//                             Map [("a", [(0.0, 2006.0)]); ("m", [(0.0, 4001.0)]); ("s", [(0.0, 1351.0)]);    ("x", [(0.0, 1416.0)])])

// map [("a", [(0.0, 2006.0)]); ("s", [(0.0, 1351.0)]); ("x", [(2662.0, 4001.0)])])
// 13 0L [15320205000000L]
// []
// [map
//    [("a", [(0.0, 2006.0)]); ("m", [(0.0, 4001.0)]); ("s", [(0.0, 1351.0)]);
//     ("x", [(0.0, 1416.0)])]]


let rec collectRules defVal flowsMap (defaultAct, conds) = 
    conds
    |> flip Seq.append [defaultAct, Map.empty]
    |> Seq.collect(fun (act, intervalsMap) -> 
            Map.tryFind act flowsMap 
            |> Option.map (collectRules defVal flowsMap >> Seq.map(second (tuple2 intervalsMap >> onMaps defVal (intersectI []))))
            |> Option.defaultValue (seq {act, intervalsMap}))
    
let collectRulesM defVal flowsMap = memoizeN (collectRules defVal flowsMap)
    
let rec combineDecisions = 
    function 
    | res, [] -> List.rev res 
    | (decision1, m1)::other1, (decision2, m2)::other2 when decision1 = decision2 ->
        combineDecisions ((decision1, onMaps [] (unionI []) (m1, m2))::other1, other2)
    | other, x1::other1 -> combineDecisions (x1::other, other1)

let countAccepted rules = 
    Seq.map(fun part -> 
        rules |> Seq.find(snd >> 
            Map.toSeq >> Seq.forall (fun (key, intervals) -> 
                let v: float = Map.find key part
                intervals |> Seq.exists (fun (s, e) -> s < v && v < e)))
        |> fst |> tuple2 part)
    >> Seq.filter(snd >> (=) "A")
    >> Seq.collect(fst >> Map.values)
    >> Seq.sum
    // >> Seq.toList

let part1 = 
    File.ReadAllText 
    >> split ["\n\n"]
    >> Seq.map (split ["\n"] >> Seq.filter (System.String.IsNullOrEmpty >> not))
    >> fanout (Seq.head >> Seq.map (parseWorkflow (-infinity) infinity) >> Map.ofSeq) 
              (Seq.last >> Seq.map parsePart)
    >> first(fanout id (Map.find "in") >> uncurry (collectRulesM [-infinity, infinity])
        >> Seq.toList)
        // >> tuple2 [] >> combineDecisions)
    >> uncurry countAccepted

// part1 "./data/d19.txt"

let excludeOne (fromRange, toExclude) =
    let intersect = onMaps [0., 4001.] (intersectI []) (fromRange, toExclude)
    if hasNoEmptyMap intersect then 
        onMapsComb [0., 4001.] (fromRange, intersect) |> Seq.filter hasNoEmptyMap 
    else [fromRange]

let rangesToCount = Seq.map(bimap int64 int64 >> uncurry (flip (-)) >> flip (-) 1L) >> Seq.sum
let mapToCount = Map.toSeq >> Seq.map (snd >> rangesToCount) >> Seq.reduce (*)


let rec exclude visited acc =
    function 
    | [] -> acc
    | (fromRange, [])::rangeExcludePairs ->
        let rangeExcludePairs = 
            List.map(fun (r, e) -> r, fromRange::e) rangeExcludePairs
        exclude Set.empty (fromRange::acc) rangeExcludePairs
    | ((fromRange, toExclude::excludes) as excl)::rangeExcludePairs ->                 
        // printfn "RANGE: %A %A %A %A" (Seq.length acc) (Seq.length rangeExcludePairs) (Seq.length excludes) (if Set.contains excl visited then "VISITED" else "")
        // let visited = Set.add excl visited
        excludeOne (fromRange, toExclude)
        // |> tap (printfn "RES: %A")
        // |> Seq.map(fun s -> 
        //     let c = mapToCount s
        //     if c = 0L then
        //         failwithf "ZERO ERROR"
        //     printf "%A" c
        //     s)        
        |> Seq.map(flip tuple2 excludes)
        // |> Seq.filter(fun p -> Set.contains p visited |> not)
        |> Seq.fold (flip List.cons) rangeExcludePairs
        // |> List.distinctBy fst
        |> exclude visited acc        

// exclude [] [Map ["a", [0., 100.; 100., infinity]]; Map ["x", [0., 100.]], Map ["a", [0, 100.]]]
// exclude (Map ["a", [-infinity, infinity]]) [Map ["a", [0., 100.; 100., infinity]]; Map ["x", [0., 100.]]]

let excludeAll (excludes, ranges) = 
    ranges |> Seq.map(flip tuple2 excludes)
    |> Seq.toList |> exclude Set.empty []

// let rec excludeAll = 
//     function 
//     | [], res -> res 
//     | _, [] -> [] 
//     | toExclude::other, ranges -> 
//         exclude toExclude ranges 
//         |> tuple2 other |> excludeAll

// let a = 
//     [Map ["a", [0., 100.]]; Map ["x", [50., 70.]]; Map ["s", [50., 70.]]]
// excludeAll (a, [Map ["a", [0., 100.; 100., 4001.]]; Map ["x", [0., 100.]]; Map ["m", [50., 70.]]])

let addMissing m = 
    ["x";"m";"a";"s"] |> Seq.fold(fun acc part -> 
        match Map.tryFind part acc with 
        | None -> Map.add part [0.,4001.] acc 
        | _ -> acc) m
// rangesToCount [(1.,2.);(1.,3.)]
let rec countAccepted2 (count: int64) exclude = 
    function 
    | [] -> count 
    | (("A", range) as r)::other ->           
        let range = addMissing range
        // let excludeR = 
        //     exclude |> Seq.map(fun e -> onMaps [0., 4001.] (intersectI []) (e, range))
        //     |> Seq.filter hasNoEmptyMap |> Seq.toList
        let finalRanges = excludeAll(exclude, [range])
        let counts = 
            finalRanges |> Seq.map(mapToCount) |> Seq.sum        
        printfn "%d" (Seq.length other)
        // printfn "%d %A [%A]\n%A\n%A" (Seq.length other) counts (count + counts) finalRanges exclude
        let exclude = range :: exclude
        if counts = 0 then count + counts 
        else countAccepted2 (count + counts) exclude other
    | (("R", range) as r)::other ->
        printfn "%d" (Seq.length other)
        let range = addMissing range
        countAccepted2 count (range::exclude) other

let part2 = 
    File.ReadAllText 
    >> split ["\n\n"]
    >> Seq.map (split ["\n"] >> Seq.filter (System.String.IsNullOrEmpty >> not))
    >> Seq.head >> Seq.map (parseWorkflow 0 4001) >> Map.ofSeq
    >> fanout id (Map.find "in") >> uncurry (collectRulesM [0, 4001])
    >> Seq.toList   
    // >> take 4
    // >> tap (printfn "%A")
    >> countAccepted2 0L []
    // >> Seq.toList
    // >> uncurry countAccepted

part2 "./data/d19.txt"

// 167409079868000
// 167409079868000
// 167409079868000
// 167409079868000
// 167409079868000