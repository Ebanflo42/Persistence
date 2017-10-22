module Parser

open System.Text.RegularExpressions

let rec parseIntList (str : string) : int list =
  let capture      = Regex.Match(str, "\d+(,|])")
  if capture       = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (int str.[init..(fin - 2)]) :: (parseIntList str.[fin..])

let rec parse2DIntList (str : string) : int list list =
  let capture = Regex.Match(str, "\[.{3,}](,|])")
  if capture  = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (parseIntList str.[init..(fin - 2)]) :: (parse2DIntList str.[fin..])

//bad and probably unnecessary
(*
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
*)

let rec printList (list : int list) : unit =
  printf "["
  let len = list.Length - 1
  for i in 1..len do
    if i = len then printfn "%i]" list.[i-1]
    else printf "%i, " list.[i-1]

let rec print2dList (list : int list list) : unit =
  match list with
    | []        -> ()
    | (x :: xs) -> printList x; print2dList xs

let rec print2dArr (data : int [] []) : unit =
  for arr in data do
    for x in arr do
      printf "%i" x
    printfn ""
