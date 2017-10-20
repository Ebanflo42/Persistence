module Calculator

open Homology

let _ = while true do
          let input = System.Console.ReadLine()
          let intArr = input |> parse2DIntArr |> finishParsing
          if intArr = [||] then printfn "Please give valid input" else printActualArr intArr
