module SimplicialComplex

open Matrix
open Chain

type SimplicialComplex(Parents : int list list,
                       Children : int list list,
                       Homology : int list list,
                       numUpdates : int) =
  member this.Parents    = Parents
  member this.Children   = Children
  member this.Homology   = Homology
  member this.numUodates = numUpdates

  member this.Dimension : int =
    Parents
      |> List.map (fun (list : int list) -> list.Length : int)
        |> fun (l : 'a list) -> List.fold max l.[0] l.[1..]

  member this.OnlyParents =

    let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
      if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (Set.[l1..]) (l2 - l1)

    let len = Parents.Length - 1
    for i in 0..len do
      for j in i..len do
        if checkSuperSet Parents.[i] Parents.[i].Length Parents.[j] Parents.[j].Length
          then failwith "The simplicial complex has children simplices that are supposed to be parent simplices"
        else ()

  member this.biggestSimplices =
    let dim = this.Dimension
    List.filter (fun l -> l.Length = dim) Parents

  member this.nDimensionalSimplices (n : int) =
    List.filter (fun l -> l.Length = n) (Parents @ Children)

  member this.Update (order : int) =
    if numUpdates = 0 then
      let data = getBoundaryOperator Parents order
      SimplicialComplex(Parents, (snd data) @ Children, ((fst data).makeHomology) :: Homology, 1)
    else
      let data = getBoundaryOperator (this.nDimensionalSimplices (Dimension - numUpdates))
      SimplicialComplex(Parents, (snd data) @ Children, ((fst data).makeHomology) :: Homology, numUpdates + 1)

  member this.CalculateHomology (order : int) =
    let dim = this.Dimension
    match numUpdates with
      | dim -> this
      | _   -> this.Update.CalculateHomology
