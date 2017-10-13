let bind (o : Option<'a>) (f : 'a -> Option<'b>) : Option<'b> =
  match o with
    | None   -> None
    | Some x -> f x

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

let checkSuperSet (set : int list) (l1 : Option<int>) (Set : int list) (l2 : Option<int>) : bool =
  match l1 with
    | None   -> let len1 = set.Length
      match l2 with
      | None -> let len2 = Set.Length
        checkSuperSet set len1 Set len2
    | Some m -> match l2 with
                 | l2 while l2 < l1 -> false
                 | _                      -> if take l1 Set = set then true
                   else checkSuperSet set l1 (drop l1 Set) (l1 - l2)

(*
type HasseDiagram(OrderedSets : int list list) =
  member this.OrderedSets = OrderedSets

  static member toNode (set : int list) =


  member this.toNodeList  =
*)
let _ = printf "A work in progress"
