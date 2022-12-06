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

  [<AutoOpen>]
  module day3 =
    let private asciiLowerCaseA = int 'a'
    
    /// calculates the priority of a character
    let private findPriority c =
      let rangeStart = if Char.IsUpper(c) then 27 else 1
      let priority = (int (Char.ToLower c)) - asciiLowerCaseA + rangeStart
      priority
      
    // find the (first) common character amongst a list of strings
    let private findCommonChar (strings: string list) =
      Array.find (fun (c: Char) ->
        List.forall (fun (s: string) -> s.Contains c) strings.Tail )
        (strings.Head.ToCharArray())
    
    let ``day3.1`` (inputStream: StreamReader) =
      streamToLines inputStream
      |> Seq.map (fun str -> str.Substring(0, str.Length/2), str.Substring(str.Length/2)) // split each string in two
      |> Seq.map (fun (s1, s2) -> findCommonChar [s1; s2])// find the first common character between both strings
      |> Seq.map findPriority // get the priority
      |> Seq.sum // sum the priorities
    
    let ``day3.2`` (inputStream: StreamReader) =
      streamToLines inputStream
      |> Seq.chunkBySize 3 // chunk into sequences of 3 strings
      |> Seq.map (fun strings -> findCommonChar (Seq.toList strings)) // find the common char in all 3 strings
      |> Seq.map findPriority // get the priority
      |> Seq.sum // sum the priorities

  [<AutoOpen>]
  module day4 =
    // converts a string of form (a-b,c-d) to (int*int)*(int*int)
    let private strToRange (str: string) =
      let split = str.Split('-')
      Convert.ToInt32 split[0], Convert.ToInt32 split[1]
      
    let ``day4.1`` (inputStream: StreamReader) =    
      // returns true iff super entirely contains sub
      let rangeContainsRange (sub: int*int) (super: int*int) =
        fst super <= fst sub && snd super >= snd sub

      streamToLines inputStream
      |> Seq.map (fun line ->
        let split = line.Split(',')
        split[0] |> strToRange, split[1] |> strToRange) // convert string to two tuples
      |> Seq.filter (fun (rng1, rng2) -> rangeContainsRange rng1 rng2 || rangeContainsRange rng2 rng1) // filter based on containment 
      |> Seq.length // count the number of pairs for which one range is fully contained within the other
    
    
    let ``day4.2`` (inputStream: StreamReader) =
      // returns true iff rng1 and rng2 are not disjoint
      let rangesIntersect rng1 rng2 =
        // 'lowest' is the lowest range
        let lowest, other =
          if fst rng1 <= fst rng2 then
            rng1, rng2
          else
            rng2, rng1
        // if the lowest element in other is lowest than the highest element in the lowest range, then we have an intersection
        fst other <= snd lowest 
       
      streamToLines inputStream
      |> Seq.map (fun line ->
        let split = line.Split(',')
        split[0] |> strToRange, split[1] |> strToRange) // convert string to two tuples
      |> Seq.filter (fun (rng1, rng2) -> rangesIntersect rng1 rng2) // filter based on containment 
      |> Seq.length // count the number of pairs for which one range is fully contained within the other
      
  [<AutoOpen>]
  module day5 =
    
    // a move command
    type private MoveCommand =
      {
        /// The index of the source column.
        SourceCol: int
        /// The index of the destination column.
        DestinationCol: int
        /// The number of items to move from the source column to the destination column.
        NumItemsToMove: int
      }
      
    /// updates the list of stacks with a given move. If pickupMany is true, assume that can move multiple items at a time (preserving order).
    let private updateStacks (pickupMany: bool) (stacks: char list list) (move: MoveCommand) =
      let src = stacks[move.SourceCol]
      let dst = stacks[move.DestinationCol]
      let pickupFunc =
        if pickupMany then
          id
        else
          List.rev
      let moved =
        src
        |> List.take move.NumItemsToMove
        |> pickupFunc
      let newSrc = (List.skip move.NumItemsToMove src)
      let newDst = (moved@dst)
      stacks
        |> List.updateAt move.SourceCol newSrc
        |> List.updateAt move.DestinationCol newDst
      
    /// The core function used by day5.1 and day5.2.
    let day5Core (pickupMany: bool) (inputStream: StreamReader) =
      let lineStream = streamToLines inputStream
      
      // get all the lines before the move commands
      let preMoveLines =
        lineStream
        |> Seq.takeWhile (fun line -> line.Trim().Length > 0)
        |> Seq.rev
        |> Seq.toList
      
      // find the column indices
      let colIndices =
        (preMoveLines |> List.head).ToCharArray()
        |> Array.mapi (fun i c -> i, c)
        |> Array.filter (fun (i, c) -> c <> ' ')
        |> Array.map fst
      
      // read the stacks
      let stackLines = List.tail preMoveLines
      let stacks = 
        seq {
          for i in colIndices do
            let mutable col = []
            for line in stackLines do
              if not (Char.IsWhiteSpace(line[i])) then 
                col <- line[i]::col
            yield col
        } |> Seq.toList
      
      // read the move operations
      let moves =
        lineStream
        |> Seq.map (fun line ->
          let split = line.Split()
          { NumItemsToMove = Convert.ToInt32 split[1]; SourceCol = Convert.ToInt32 split[3] - 1; DestinationCol = Convert.ToInt32 split[5] - 1; })
        |> Seq.toList
      
      // apply the move commands to the stacks
      moves
      |> List.fold
          (updateStacks pickupMany)
          stacks
      |> List.map (fun lst -> lst.Head.ToString())
      |> String.concat ""
      
    let ``day5.1`` (inputStream: StreamReader) = day5Core false inputStream
    let ``day5.2`` (inputStream: StreamReader) = day5Core true inputStream
  
  [<AutoOpen>]
  module dayN =
    let ``dayN.1`` (inputStream: StreamReader) = ()
    let ``dayN.2`` (inputStream: StreamReader) = ()