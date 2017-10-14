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
let rec pow10 n = match n with
                    | 0 -> 1
                    | 1 -> 10
                    | 2 -> 100
                    | 3 -> 1000
                    | 4 -> 10000
                    | 5 -> 100000
                    | 6 -> 1000000
                    | 7 -> 10000000
                    | 8 -> 100000000
                    | 9 -> 1000000000
                    | _ -> 10*pow10 (n - 1)

let rec strToInt (n : int) (str : string) : int =
  if n < 1 then 0 else
    let rec getDigit c = match c with
                           | '0' -> 0
                           | '1' -> 1
                           | '2' -> 2
                           | '3' -> 3
                           | '4' -> 4
                           | '5' -> 5
                           | '6' -> 6
                           | '7' -> 7
                           | '8' -> 8
                           | '9' -> 9
                           | _   -> -1
    let digit = getDigit str.[0]
    if digit = -1 then -1 else digit*(pow10 n) + strToInt (n - 1) str.[1..]

//let parse (str : string) : HasseDiagram =

let rec main : unit = let input = System.Console.ReadLine()
                      let i = strToInt input.Length input
                      if i = -1 then printf "Please give valid input"; main else printfn "%i" i; main

let _ = main
