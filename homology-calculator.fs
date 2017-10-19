//UTIL
let bind (o : Option<'a>) (f : 'a -> Option<'b>) : Option<'b> =
  match o with
    | None   -> None
    | Some x -> f x

let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
  if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (Set.[l1..]) (l2 - l1)

let rec checkSubSet (Set : int list) (l1 : int) (set : int list) (l2 : int) : bool =
  checkSuperSet set l2 Set l1

let rec maximum (arr : int []) : int = Array.fold max arr.[0] arr.[1..]

let arrMap (f : 'a -> 'b) (arr : 'a []) : 'b [] = [|for x in arr -> f x|]

let toResizeArray (l : 'a list) =
  let result = new ResizeArray<'a>()
  for elem in l do
    result.Add(elem)
  result

let rec drop (n : int) (list : 'a list) : 'a list =
  match list with
    | [] -> []
    | _  -> if n > 0 then drop (n - 1) (List.tail list) else list

let removeAt (n : int) (list : 'a list) : 'a list = (List.take (n - 1) list) @ (drop n list)

let rec findIndices (arr : 'a []) (elem : 'a) (index : int) : int list =
  if arr = [||] then []
  elif arr.[0] = elem then index :: (findIndices arr.[1..] elem (index + 1))
  else findIndices arr.[1..] elem (index + 1)

let rec findAndReplace (i : int) (elem : 'a) (list : 'a list list) : 'a list =
  [|for n in 0..list.Length - 1 ->
    if n = i then elem :: list.[n]
    else list.[n]|] //Arrangement could confuse compiler

let rec reduceArr (input : 'a []) : 'a [] * int [] [] =

  let rec process (data : 'a list) (ans : 'a list) (indices : int list list) (index : int) : 'a list * int list list =
    match data with
      | []         -> (ans, indices)
      | (x :: xs)  ->
        match List.tryFindIndex x ans with
          | None   -> process xs (x :: ans) indices (index + 1)
          | Some i ->
            replaceAndReplace i index indices
              |> fun a -> process xs ans a (index + 1)
(*
  let rec getReduced (soln : 'a list) (helper : 'a list) (indices : int list list) : 'a list * int list list =
    match helper with
      | []        -> (soln, indices)
      | (x :: xs) -> getReduced (x :: soln) (List.filter (fun a -> a != x) helper)
*)
  let soln   = process [|for elem in input -> elem|] [] [] 0
  let len    = soln.Length - 1
  let result = (Arrary.create len, Arrary.create len)
  for i in 0..len do
    (fst result).[i] = (fst soln).[i]
    let s            = (snd soln).[i]
    let len1         = s.Length - 1
    let arr          = Array.create len1
    for j in 0..len1 do
      arr.[j] = s.[j]
    (snd result).[i] = arr
  result


(*
let rec factorial (n : int) =
  match n with
    | 0 -> 1
    | n -> n * factorial (n - 1)

let rec binomialCoeff (n : int) (k : int) : int =
  let rec ordChoice (i : int) (j : int) : int =
    match j with
      | 0 -> 1
      | 1 -> i
      | 2 -> i*(i - 1)
      | 3 -> i*(i - 1)*(i - 2)
      | _ -> (i - j + 1)*ordChoice i (j - 1)
  (ordChoice n k)/(factorial k)

let rec getSubLists (len : int) (list : int list) : int list [] =
  let helper = []
  let ssLen = list.Length
  let result = [|for i in 1..(binomialCoeff ssLen len) -> helper|]

  let rec findSubLists (current : int list) (guess : int) (b : bool) (iter : int) : unit =
    if b && current.Length = len then result.[iter] = current; findSubLists current guess false (iter + 1)
    elif guess < ssLen then
      let x = list.[guess]
      findSubLists (x :: current) (guess + 1) true iter
      findSubLists (removeAt x current) (guess + 1) true iter
    else ()

  findSubLists helper 0 true 0
  result
*)
let rec getImmediateSubArrs (arr : int []) : int [] [] = [|for i in 0..arr.Length - 1 -> Array.append arr.[0..i] arr.[i+1..]|]

(*
let rec getSubLists (len : int) (list : int list) (guess : int) (current : int list) (soln : int list list) (b : bool) : int list list =
  if b && current.Length = len then
    let newsoln = current :: soln
    getSubLists len list guess current newsoln false
  elif guess = list.Length then
    let x = list.[guess] //index out of bounds exception?
    let next = x :: current
*)

//DATA TYPES
type HasseNode(Vertices : int [], Parents : HasseNode [], Children : HasseNode []) =
  member this.Vertices = Vertices
  member this.Parents  = Parents
  member this.Children = Children

  member this.checkNodeChilds =
    let dim = Vertices.Length
    if dim = 1 && Children = [||] then true
    else Children
      |> arrMap (fun nodes -> nodes.Vertices.Length)
        |> Array.forall (fun x -> x = dim - 1)

  member this.checkNodeParens =
    if Parents = [||] then true
    else Parents
      |> arrMap (fun nodes -> nodes.Vertices.Length)
        |> Array.forall (fun x -> x = Vertices.Length + 1)

  member this.checkNode =
    this.checkNodeChilds && this.checkNodeParens

type SimplicialComplex(OrderedSets : int [] []) =
  member this.OrderedSets = OrderedSets

  member this.Dimension : int = maximum (arrMap (fun (arr : int []) -> arr.Length : int) OrderedSets)

  //member this.BiggestSimplices : int list list = List.filter (fun (l : 'a list) -> l.Length = this.Dimension) this.OrderedSets
(*
  member this.Nodes : HasseNode list =
    let fstLevelNodes = arrMap (fun arr -> HasseNode arr [||]) OrderedSets
    let rec makeNodes (seed : HasseNode [] [] -> HasseNode []) (sets : int [] []) =
      let nextLevelNodes = arrMap (fun arr -> HasseNode )
*)

type Chain(Simplices : int [] [], Coefficients : int [], Dimension : int, Order : int, isReduced : bool) =
  member this.Simplices    = Simplices
  member this.Coefficients = Coefficients
  member this.Order        = Order
  member this.isReduced    = isReduced

  member this.isValid : bool =
    Simplices.Length = Coefficients.Length && Array.forall (fun (arr : int []) -> arr.Length = Dimension) Simplices

  member this.reduce =
    if isReduced then this
    else
      let newSimplices =


  member this.Boundaries : Chain [] =


//PARSING
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

//TESTING
let rec main : unit = while true do
                        let input = System.Console.ReadLine()
                        let intArr = input |> parse2DIntArr |> finishParsing
                        if intArr = [||] then printfn "Please give valid input" else printActualArr intArr

let _ = main
