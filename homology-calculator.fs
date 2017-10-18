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


//PARSING
open System.Text.RegularExpressions

let rec parseIntList (str : string) : int list =
  let capture      = Regex.Match(str, "\d+(,|])")
  if capture       = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (int str.[init..(fin - 2)]) :: (parseIntList str.[fin..])

let rec parseIntListList (str : string) : int list list =
  let capture = Regex.Match(str, "\[.{3,}](,|])")
  if capture  = Match.Empty then [] else
    let init : int = capture.Index
    let fin : int  = init + capture.Length
    (parseIntList str.[init..(fin - 2)]) :: (parseIntListList str.[fin..])

let rec printArr (list : int list) : unit =
  for n in list do
    printf "%i " n
  printfn ""

let rec print2DArr (list : int list list) : unit =
  match list with
    | []        -> printfn ""
    | (x :: xs) -> printArr x; printfn ""; (print2DArr xs)

//TESTING
let rec main : unit = while true do
                        let input = System.Console.ReadLine()
                        let intList = parseIntListList input
                        if intList = [] then printfn "Please give valid input" else print2DArr intList

let _ = main
