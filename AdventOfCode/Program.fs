open System
open System.IO
open AdventOfCode.Solutions

let day = Environment.GetCommandLineArgs()[1]
let problem = Environment.GetCommandLineArgs()[2]

let solution =
  match day, problem with
  | "1", "1" -> ``day1.1``
  | "1", "2" -> ``day2.2``
  | "2", "1" -> ``day2.1``
  | "2", "2" -> ``day2.2``
  | "3", "1" -> ``day3.1``
  | "3", "2" -> ``day3.2``
  | "4", "1" -> ``day4.1``
  | "4", "2" -> ``day4.2``
  | unknownInput -> failwithf $"Unknown input {unknownInput}"

let inputFile = $"day{day}.txt"
let stream = new StreamReader(inputFile)
printfn $"{solution stream}"
stream.Dispose()