module SimplicialComplex

let rec maximum (list : int list) : int = List.fold max list.[0] list.[1..]

type SimplicialComplex(OrderedSets : int list list) =
  member this.OrderedSets = OrderedSets

  member this.Dimension : int = maximum (List.map (fun (list : int list) -> list.Length : int) OrderedSets)
