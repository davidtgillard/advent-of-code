open System
open System.IO
open AdventOfCode.Solutions

let day = Environment.GetCommandLineArgs()[1]
let problem = Environment.GetCommandLineArgs()[2]

let toString (func: StreamReader -> 't) (reader: StreamReader) =
  (func reader).ToString()

let solution =
  match day, problem with
  | "1", "1" -> toString ``day1.1``
  | "1", "2" -> toString ``day2.2``
  | "2", "1" -> toString ``day2.1``
  | "2", "2" -> toString ``day2.2``
  | "3", "1" -> toString ``day3.1``
  | "3", "2" -> toString ``day3.2``
  | "4", "1" -> toString ``day4.1``
  | "4", "2" -> toString ``day4.2``
  | "5", "1" -> toString ``day5.1``
  | "5", "2" -> toString ``day5.2``
  | unknownInput -> failwithf $"Unknown input {unknownInput}"

let inputFile = $"day{day}.txt"
let stream = new StreamReader(inputFile)
printfn $"{solution stream}"
stream.Dispose()