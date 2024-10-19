open System 
open System.IO 
let split (sep: string) (x:string) = x.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let inline fork f1 f2 x = (f1 x, f2 x)
let inline (||>>) f1 f2 x = f1 x ||> f2

let walk initPos endPos (inst: string) maze = 
    let rec makeStep numSteps stepId curPos = 
        if endPos numSteps stepId curPos then numSteps
        else 
            let nextPos =  Map.find curPos maze |> (Seq.item stepId inst |> function 'L' -> fst | _ -> snd)
            let nextStepId = if stepId + 1 = Seq.length inst then 0 else stepId + 1            
            makeStep (numSteps + 1L) nextStepId nextPos
    makeStep 0L 0 initPos

let common walk =
    File.ReadAllText 
    >> split "\n\n"
    >> fork Seq.head
        (Seq.last 
         >> split "\n"
         >> Seq.map(split " = "
            >> fork Seq.head
                (Seq.last >> split ", "
                    >> fork (Seq.head >> Seq.tail >> Seq.toArray >> String)
                            (Seq.last >> Seq.filter ((<>) ')') >> Seq.toArray >> String)))
         >> Map)
    ||>> walk

//part1
// let part1 = common <| walk "AAA" (fun _ _ -> (=) "ZZZ")
// part1 "./data/d8.txt"

let rec dividers tryNum acc num = 
    if num % tryNum = 0 then 
        let num = num / tryNum 
        let acc = tryNum::acc 
        if num = 1 then acc 
        else dividers tryNum acc num 
    else dividers (tryNum + 1) acc num

let part2 = 
    common
    <| fun inst maze ->
        let starts = (Map.keys >> Seq.filter (Seq.last >> (=) 'A')) maze
        starts |> Seq.map(fun s -> 
            let mutable shiftsAndPeriods = Map.empty
            walk s (fun curStep stepId curPos -> 
                if (Seq.last >> (=) 'Z') curPos then    
                    let key = curPos + ":" + inst[stepId..] + inst[..stepId-1]
                    match Map.tryFind key shiftsAndPeriods with 
                    | None -> 
                        shiftsAndPeriods <- Map.add key (curStep, 0L) shiftsAndPeriods
                        false
                    | Some (prevStep, 0L) -> 
                        shiftsAndPeriods <- Map.add key (prevStep, curStep) shiftsAndPeriods
                        shiftsAndPeriods |> Map.values |> Seq.map snd |> Seq.forall ((<) 0L) 
                    | _ -> false
                else false) inst maze |> ignore            
            shiftsAndPeriods 
            |> Map.values |> Seq.map(fork fst (fork snd fst ||>> (-)))
        )
        |> Seq.map (Seq.head >> fst >> int >> dividers 2 [] >> Set.ofSeq)
        |> Seq.reduce Set.union
        |> Seq.map int64 |> Seq.reduce (*)

part2 "./data/d8.txt"