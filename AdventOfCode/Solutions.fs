module AdventOfCode.Solutions

open System
open System.IO

let private streamToLines (inputStream: StreamReader) =
  let rec getLines (s: StreamReader) =
    seq {
      let line = inputStream.ReadLine()
      if line <> null then
        yield line
        yield! getLines s
    }
  getLines inputStream

[<AutoOpen>]
module Day1 =
  let private groupByElf (inputStream: StreamReader) = 
    Seq.foldBack (fun (line: string) (ll: int list list) ->
                      let mutable result = 0
                      if Int32.TryParse(line, &result) then
                        (result::ll.Head)::ll.Tail
                      else
                        []::ll
                    )
                    (streamToLines inputStream)
                    [[]]
    |> List.map List.sum
  
  let ``day1.1`` (inputStream: StreamReader) = 
    groupByElf inputStream
    |> List.max
    
  let ``day2.2`` (inputStream: StreamReader) = 
    groupByElf inputStream
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.sum
 
[<AutoOpen>]
module Day2 = 
  type private RpsType =
    | Rock = 1
    | Paper = 2
    | Scissors = 3
    
  let numRpsTypeCases = Enum.GetNames<RpsType>().Length
    
  let private getRpsWinner (rps: RpsType): RpsType =
    let m =
      let x = ((int rps + 1) % numRpsTypeCases)
      if x = 0 then numRpsTypeCases else x
    LanguagePrimitives.EnumOfValue m
   
  let private getRpsLoser (rps: RpsType): RpsType =
    let m =
      let x = ((int rps - 1) % 3)
      if x = 0 then numRpsTypeCases else x
    LanguagePrimitives.EnumOfValue m
   
  let private rpsScore opponents yours =
    let roundScore =
      match yours, opponents with
      | RpsType.Rock, RpsType.Paper -> 0
      | RpsType.Rock, RpsType.Scissors -> 6
      | RpsType.Paper, RpsType.Rock -> 6
      | RpsType.Paper, RpsType.Scissors -> 0
      | RpsType.Scissors, RpsType.Rock -> 0
      | RpsType.Scissors, RpsType.Paper -> 6
      | y, o when y = o -> 3 // draw
      | _ -> failwithf "case not covered"
    int yours + roundScore // return the value
    
  let private charStrToRps str =
    match str with
    | "A" | "X" -> RpsType.Rock
    | "B" | "Y" -> RpsType.Paper
    | "C" | "Z" -> RpsType.Scissors
    | _ -> failwithf "case not covered"
    
  type private RpsOutcome =
    | Lose = 0
    | Draw = 3
    | Win = 6
      
  let private charStrToRpsOutcome str =
    match str with
    | "X" -> RpsOutcome.Lose
    | "Y" -> RpsOutcome.Draw
    | "Z" -> RpsOutcome.Win
    | _ -> failwithf "case not covered"
      
  let ``day2.1`` (inputStream: StreamReader) =
    streamToLines inputStream
    |> Seq.map (fun line -> // first, convert each line to two rpsTypes
      let split = line.Split(' ')
      charStrToRps split[0], charStrToRps split[1]) // convert to tuple of RpsTypes
    |> Seq.map (fun (opponents, yours) -> rpsScore opponents yours) // determine the scores of each match
    |> Seq.sum // sum the scores
    
  let ``day2.2`` (inputStream: StreamReader) =
    streamToLines inputStream
    |> Seq.map (fun line -> // first, convert each line to two to rpsTypes
      let split = line.Split(' ')
      charStrToRps split[0], charStrToRpsOutcome split[1]) // convert to tuples of form 'opponent type' * 'indented outcome'
    |> Seq.map (fun (opponents, intendedOutcome) ->
      let yours = // determined the choice of RPS type based on intended outcome
        match intendedOutcome with
        | RpsOutcome.Draw -> opponents
        | RpsOutcome.Lose -> getRpsLoser opponents
        | RpsOutcome.Win -> getRpsWinner opponents
        | _ -> failwithf "case not covered"
      rpsScore opponents yours)
    |> Seq.sum


