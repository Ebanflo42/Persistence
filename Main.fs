module Main

open Parser
open Matrix
open Chain
open SimplicialComplex

let _ = while true do

          let input  = System.Console.ReadLine()
          //let sc     = SimplicialComplex.defaultParse input
          let output = parse2dIntList input

          //sc.printAttribs
          if output = [] then printfn "Please give valid input"
          else print2dList output
