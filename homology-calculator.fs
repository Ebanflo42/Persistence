//UTIL
let bind (o : Option<'a>) (f : 'a -> Option<'b>) : Option<'b> =
  match o with
    | None   -> None
    | Some x -> f x

let rec drop (n : int) (list : List<'a>) =
  List.tail (drop (n - 1) (List.tail list))

let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
  if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (drop l1 Set) (l2 - l1)

let rec checkSubSet (Set : int list) (l1 : int) (set : int list) (l2 : int) : bool =
  checkSuperSet set l2 Set l1

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
  member this.OrderedSets = OrderedSets

  member this.getNthChainRank (n : int) : int =
    let rec numSets (sets : int list list) = match sets with
                                          | []        -> 0
                                          | (x :: xs) -> if x.Length = n then 1 + numSets xs else numSets xs
    numSets OrderedSets

let _ = printf "A work in progress"
