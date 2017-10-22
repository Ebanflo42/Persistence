module Main

open Parser

let _ = while true do

          let input  = System.Console.ReadLine()
          let output = input |> parse2DIntList

          if output = [] then printfn "Please give valid input"
          else print2dList output
