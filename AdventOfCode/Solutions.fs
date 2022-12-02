module AdventOfCode.Solutions

open System
open System.IO

let streamToLines (inputStream: StreamReader) =
  let rec getLines (s: StreamReader) =
    seq {
      let line = inputStream.ReadLine()
      if line <> null then
        yield line
        yield! getLines s
    }
  getLines inputStream
    
let ``dayOne.1`` (inputStream: StreamReader) = 
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
  |> List.max
  
let ``dayOne.2`` (inputStream: StreamReader) = 
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
  |> List.sort
  |> List.rev
  |> List.take 3
  |> List.sum



