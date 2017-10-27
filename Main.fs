module Main

open Parser
open Matrix

let _ = while true do

          let input  = System.Console.ReadLine()
          (*
          let output =
            input
              |> parse2dIntList
                |> fun (l : int list ist) -> [|for i in l -> [|for j in i -> j|]|]
                  |> fun (m : int [] [])
            *)
          let output =
            input
              |> parseIntList
                |> fun (l : int list) -> extEucAlg l.[0] l.[1]

          printList [for i in output -> i]
(*
          if output = [] then printfn "Please give valid input"
          else print2dList output
*)
