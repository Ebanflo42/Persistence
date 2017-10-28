module SimplicialComplex

open Parser
open Matrix
open Chain

type SimplicialComplex(Parents    : int list list,
                       Children   : int list list,
                       //Homology : int list list,
                       Dimension  : int,
                       numUpdates : int) =
  member this.Parents    = Parents
  member this.Children   = Children
  //member this.Homology   = Homology
  member this.Dimension  = Dimension
  member this.numUpdates = numUpdates

  static member defaultParse (str : string) =
    fst (SimplicialComplex(parse2dIntList str, [], 0, 0).CheckDimension)

  member this.PrintAttribs =
    print2dList Parents
    print2dList Children
    printfn "%i" Dimension
    printfn "%i" numUpdates

  member this.CheckDimension : SimplicialComplex * bool =
    let dim =
      Parents
        |> List.map (fun (list : int list) -> list.Length : int)
          |> fun (l : int list) -> List.fold max l.[0] l.[1..]
    if dim = Dimension
    then (this, true)
    else (SimplicialComplex(Parents, Children, numUpdates, dim), false)

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
    List.filter (fun (l : int list) -> l.Length = dim) Parents

  member this.nDimensionalSimplices (n : int) =
    List.filter (fun (l : int list) -> l.Length = n) (Parents @ Children)
(*
  member this.Update (order : int)  : SimplicialComplex =
    if numUpdates = 0 then
      let data = getBoundaryOperator Parents order
      SimplicialComplex(Parents, (snd data) @ Children, (*(fst data).makeHomology) :: Homology,*) Dimension, 1)
    else
      let data = getBoundaryOperator (this.nDimensionalSimplices (Dimension - numUpdates)) order
      SimplicialComplex(Parents, (snd data) @ Children, (*(fst data).makeHomology) :: Homology,*) Dimension, numUpdates + 1)

  member this.CalculateHomology (order : int) =
    let dim = this.Dimension
    match numUpdates with
      | dim -> this
      | _   -> (this.Update order).CalculateHomology order
*)