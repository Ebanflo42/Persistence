module Parser

open System.Text.RegularExpressions

let rec parseIntList (str : string) : int list =
  let capture      = Regex.Match(str, "\d+(;|])")
  if capture       = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (int str.[init..(fin - 2)]) :: (parseIntList str.[fin..])

let rec parse2dIntList (str : string) : int list list =
  let capture = Regex.Match(str, ".+?(?=])].")
  if capture  = Match.Empty then [] else
  let init : int = capture.Index + 1
  let fin : int  = init + capture.Length - 1
  (parseIntList str.[init..(fin - 2)]) :: (parse2dIntList str.[fin..])

let rec printList (list : int list) : unit =
  for i in list do
    printf "%i " i
  printfn ""

let rec print2dList (list : int list list) : unit =
  match list with
    | []        -> ()
    | (x :: xs) -> printList x; print2dList xs

let rec print2dArr (data : int [] []) : unit =
  for arr in data do
    for x in arr do
      printf "%i" x
    printfn ""
