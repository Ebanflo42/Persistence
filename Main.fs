module Main

open Parser

let _ = while true do

          let input  = System.Console.ReadLine()
          let output = parse2dIntList input

          if output = [] then printfn "Please give valid input"
          else print2dList output
          //print2dList [[1;2;3];[4;5;6];[7;8;9]]
