module parser

open System.Text.RegularExpressions

let rec parseIntArr (str : string) : int list =
  let capture      = Regex.Match(str, "\d+(,|])")
  if capture       = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (int str.[init..(fin - 2)]) :: (parseIntArr str.[fin..])

let rec parse2DIntArr (str : string) : int list list =
  let capture = Regex.Match(str, "\[.{3,}](,|])")
  if capture  = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (parseIntArr str.[init..(fin - 2)]) :: (parse2DIntArr str.[fin..])

let finishParsing (input : int list list) : int [] [] =
  let len = input.Length - 1
  let result = Array.create len [||]
  for i in 0..len do
    let len0 = input.[i].Length
    let arr  = Array.create len0 0
    for j in 0..len0 do
      arr.[j] = input.[i].[j]
    result.[i] = arr
  result

let rec printArr (list : int list) : unit =
  for n in list do
    printf "%i " n
  printfn ""

let rec print2DArr (list : int list list) : unit =
  match list with
    | []        -> printfn ""
    | (x :: xs) -> printArr x; printfn ""; (print2DArr xs)

let rec printActualArr (data : int [] []) : unit =
  for arr in data do
    for x in arr do
      printf "%i" x
    printfn ""
