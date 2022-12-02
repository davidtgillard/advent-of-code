open System
open System.IO
open AdventOfCode.Solutions

let day = Environment.GetCommandLineArgs()[1]
let problem = Environment.GetCommandLineArgs()[2]

let solution =
  match day, problem with
  | "1", "1" -> ``dayOne.1``
  | "1", "2" -> ``dayOne.2``
  | unknownInput -> failwithf $"Unknown input {unknownInput}"

let inputFile = $"day{day}.txt"
let stream = new StreamReader(inputFile)
printfn $"{solution stream}"
stream.Dispose()