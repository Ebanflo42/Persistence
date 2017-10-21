module SimplicialComplex

open Matrix
open Chain

type SimplicialComplex(OrderedSets : int list list) =
  member this.OrderedSets = OrderedSets

  member this.Dimension : int =
    OrderedSets
      |> List.map (fun (list : int list) -> list.Length : int)
        |> fun (l : 'a list) -> List.fold max l.[0] l.[1..]

  member this.OnlyParents =

  let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
    if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (Set.[l1..]) (l2 - l1)

  let len = OrderedSets.Length - 1
  for i in 0..len do
    for j in i..len do
      if checkSuperSet OrderedSets.[i] OrderedSets.[i].Length OrderedSets.[j] OrderedSets.[j].Length
        then failwith "The simplicial complex has children simplices that are supposed to be parent simplices"
      else ()
