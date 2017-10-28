module Main

open Parser
open Matrix
open Chain
open SimplicialComplex

let _ = while true do

          let input  = System.Console.ReadLine()
          let matrix =
            input
              |> parse2dIntList
                |> fun (l : int list list) -> [|for i in l -> [|for j in i -> j|]|]
                  |> fun (m : int [] []) -> Matrix(m, 0)

          let pivot = matrix.FindPivot

          printfn "%i %i" (fst pivot) (snd pivot)
