//UTIL
let bind (o : Option<'a>) (f : 'a -> Option<'b>) : Option<'b> =
  match o with
    | None   -> None
    | Some x -> f x

let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
  if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (Set.[l1..]) (l2 - l1)

let rec checkSubSet (Set : int list) (l1 : int) (set : int list) (l2 : int) : bool =
  checkSuperSet set l2 Set l1

let rec maximum (list : int list) : int = List.fold max list.[0] (List.tail list)

//DATA TYPES
type HasseNode(Vertices : int list, Parents : HasseNode list, Children : HasseNode list) =
  member this.Vertices = Vertices
  member this.Parents  = Parents
  member this.Children = Children

  static member checkNodeChilds (node : HasseNode) : bool =
    List.forall (fun (list : int List) -> list.Length = node.Vertices.Length - 1)
      (List.map (fun (n : HasseNode) -> n.Vertices) (node.Children : HasseNode list))

  static member checkNodeParens (node : HasseNode) : bool =
    List.forall (fun (list : int list) -> list.Length = node.Vertices.Length + 1)
      (List.map (fun (n : HasseNode) -> n.Vertices) (node.Children : HasseNode list))

  static member checkNode node =
    HasseNode.checkNodeChilds node && HasseNode.checkNodeParens node

type SimplicialComplex(OrderedSets : int list list) =
  member this.OrderedSets : int list list = OrderedSets

  member this.getNthChainRank (n : int) : int =
    let rec numSets (sets : int list list) = match sets with
                                          | []        -> 0
                                          | (x :: xs) -> if x.Length = n then 1 + numSets xs else numSets xs
    numSets OrderedSets

  member this.Dimension : int = maximum (List.map (fun (list : int list) -> list.Length) OrderedSets)

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

(*
let rec breakList (cs : char list) (len : int) (c : char) : char list list =
  printfn "%s" (new string [|for ch in cs -> ch|])
  match List.tryFindIndex (fun x -> x = c) cs with
    | None   -> [cs]
    | Some i ->
      printfn "%i" i
      let substring1 = cs.[0..i]
      printfn "%s" (new string [|for ch in substring1 -> ch|])

      let substring2 = cs.[(i + 1) .. (len - 1)]
      printfn "%s" (new string [|for ch in substring2 -> ch|])

      cs.[0..i] :: (breakList cs.[(i + 1) .. (len - 1)] (len - i) c)
*)
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
