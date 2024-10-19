open System.IO
#r "nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions

// let one signal = seq { signal }

// [printf "%A"; printf "%A"] <*> [true;false]

let sendFrom name signal = Seq.map(flip tuple2 (name, signal))

let broadcaster outs (_, signal) = sendFrom "broadcaster" signal outs

let flipFlop name = 
    let mutable state = false //off 
    fun outs -> function  
        | _, true -> 
            // printfn "%s High signal ignored" name
            Seq.empty
        | _, false -> 
            state <- not state 
            outs |> sendFrom name state
    
let mutable simulationTime = 0
let mutable conjHistory: Map<string, (int*bool) list> = Map.empty

let conj name ins  = 
    let mutable state = ins |> Seq.map(flip tuple2 false) |> Map.ofSeq 
    // let stateGetter () =
    //     state |> Map.toSeq |> Seq.map (snd >> function false -> '0' | _ -> '1') |> Seq.toArray |> System.String
    // printfn "%s: %s" name s
    // conjStates <- Map.add name stateGetter stateGetters    
    fun outs (inName, signal) ->
        state <- Map.add inName signal state 
        let signal = state |> Map.toSeq |> Seq.forall snd |> not
        conjHistory <- 
            Map.add name ((simulationTime, signal)::(Map.tryFind name conjHistory |> Option.defaultValue [])) conjHistory
        // if not signal && Map.count state > 1 then 
        //     printfn "CONJ %s" name
        outs |> sendFrom name signal


let buidMod (tp, str) ins outs = 
    Map.find str outs |>
    match tp with 
    | "&" -> conj str (Map.find str ins)
    | "%" -> flipFlop str
    | "broadcaster" -> broadcaster


let splitTypeName str = match take 1 str with ("&" | "%") as tp -> tp, skip 1 str | _ -> str, str

let rec signalSeq acc allMods =
    function
    | [] -> acc
    | (toMod, signal)::other ->
        // printfn "%s --%A--> %s | %A" (fst signal) (snd signal) toMod other
        match Map.tryFind toMod allMods with 
        | None ->             
            signalSeq acc allMods other
        | Some m -> 
            let newSignals = m signal |> Seq.toList            
            let acc = newSignals |> Seq.fold(fun acc (_, (_, level)) -> 
                if level then second ((+) 1L) acc 
                else first ((+) 1L) acc) acc
            signalSeq acc allMods (other @ newSignals)

let rec signalSeq2 allMods =
    function
    | [] -> false
    | (toMod, signal)::_ when toMod = "rx" && snd signal = false -> true 
    | (toMod, signal)::other ->
        // printfn "%s --%A--> %s | %A" (fst signal) (snd signal) toMod other
        match Map.tryFind toMod allMods with 
        | None -> signalSeq2 allMods other
        | Some m -> 
            let newSignals = m signal |> Seq.toList            
            signalSeq2 allMods (other @ newSignals)

let part1 = 
    File.ReadAllLines
    >> Seq.map(split [" -> "] >>
        fanout (Seq.head >> splitTypeName) (Seq.last >> split [", "] >> Seq.toList))   
    >> fanout (Seq.collect(first (flip tuple2 >> List.singleton) >> uncurry List.apply)
                >> Seq.groupBy fst >> Seq.map (second (Seq.map (snd >> snd) >> Seq.toList)) >> Map.ofSeq) 
                Map.ofSeq
    >> fanout (snd >> Map.keys) (second (Map.toSeq >> Seq.map (first snd) >> Map.ofSeq))
    >> fun (mods, (ins, outs)) ->
        let allMods = mods |> Seq.map(fun tpName -> snd tpName, buidMod tpName ins outs) |> Map.ofSeq
        Seq.init 1000 (fun i ->
            printfn "Starting %d" i
            signalSeq (1L, 0L) allMods [("broadcaster", ("button", false))])            
        |> Seq.reduce (++)
        |> uncurry (*)


// part1 "./data/d20.txt"

let part2 = 
    File.ReadAllLines
    >> Seq.map(split [" -> "] >>
        fanout (Seq.head >> splitTypeName) (Seq.last >> split [", "] >> Seq.toList))   
    >> fanout (Seq.collect(first (flip tuple2 >> List.singleton) >> uncurry List.apply)
                >> Seq.groupBy fst >> Seq.map (second (Seq.map (snd >> snd) >> Seq.toList)) >> Map.ofSeq) 
                Map.ofSeq
    >> fanout (snd >> Map.keys) (second (Map.toSeq >> Seq.map (first snd) >> Map.ofSeq))
    >> tap (fun _ -> conjHistory <- Map.empty)
    >> fun (mods, (ins, outs)) ->
        let allMods = mods |> Seq.map(fun tpName -> snd tpName, buidMod tpName ins outs) |> Map.ofSeq
        Seq.init 100000 id 
        |> Seq.iter (fun i ->
            simulationTime <- i + 1
            signalSeq2 allMods [("broadcaster", ("button", false))] |> ignore)
        // |> Seq.toList
        // |> Seq.find Option.isSome |> ignore 


part2 "./data/d20.txt"


conjHistory
|> Map.toSeq |> Seq.map(second(Seq.filter snd >> Seq.map fst >> Seq.rev))
|> Seq.map(second(Seq.truncate 3 >> Seq.pairwise >> Seq.map(uncurry (flip (-)))))
|> Seq.toList

conjHistory
|> Map.toSeq |> Seq.map(second(Seq.filter snd >> Seq.map fst >> Seq.rev))
|> Seq.map(second(Seq.truncate 3))
|> Seq.toList


// [("br", seq [3877; 7754; 11631]); ("fk", seq [4079; 8158; 12237]);
// ("fz", seq [1; 2; 2]); ("lb", seq [1; 1; 1]);
// ("lf", seq [3911; 7822; 11733]); ("pn", seq [1; 2; 2]);
// ("rz", seq [4057; 8114; 12171]); ("th", seq [1; 2; 3]);
// ("vd", seq [1; 2; 3])]

//   [("br", seq [3877; 3877]); ("fk", seq [4079; 4079]); ("fz", seq [1; 0]);
//    ("lb", seq [0; 0]); ("lf", seq [3911; 3911]); ("pn", seq [1; 0]);
//    ("rz", seq [4057; 4057]); ("th", seq [1; 1]); ("vd", seq [1; 1])]


let rec dividers tryNum acc num = 
    if num % tryNum = 0L then 
        let num = num / tryNum 
        let acc = tryNum::acc 
        if num = 1L then acc 
        else dividers tryNum acc num 
    else dividers (tryNum + 1L) acc num
    
[3877L; 4079L; 3911L; 4057L] |> Seq.map(dividers 2L [] >> set) |> Seq.reduce Set.union |> Seq.reduce (*)

//part2 =  250924073918341L
