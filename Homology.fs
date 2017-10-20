module Homology

let rec maximum (list : int list) : int = List.fold max list.[0] list.[1..]

type HasseNode(Vertices : int list, Parents : HasseNode list, Children : HasseNode list) =
  member this.Vertices = Vertices
  member this.Parents  = Parents
  member this.Children = Children

  member this.checkNodeChilds =
    let dim = Vertices.Length
    if dim = 1 && Children = [] then true
    else Children
      |> List.map (fun nodes -> nodes.Vertices.Length)
        |> List.forall (fun x -> x = dim - 1)

  member this.checkNodeParens =
    if Parents = [] then true
    else Parents
      |> List.map (fun nodes -> nodes.Vertices.Length)
        |> List.forall (fun x -> x = Vertices.Length + 1)

  member this.checkNode =
    this.checkNodeChilds && this.checkNodeParens

type Matrix(Elements : int [] [], Order : int) =
  member this.Elements = Elements
  member this.Order    = Order

  member this.isMatrix =
    Array.forall (fun (a : 'a []) -> a.Length = Elements.[0].Length) Elements.[1..]

type Chain(Simplices : int list list, Coefficients : int list, Dimension : int, Order : int, IsReduced : bool) =
  member this.Simplices    = Simplices
  member this.Coefficients = Coefficients
  member this.Order        = Order
  member this.IsReduced    = IsReduced

  member this.VerifyDimension : bool =
    Simplices
      |> List.map (fun list -> list.Length)
        |> List.forall (fun a -> a = Dimension + 1)
(*
  member this.VerifyAlgebraicOrder : Chain * bool =
    let mutable status : bool = true
    let len = Coefficients.Length - 1
    let newCoeffs = Array.create len

    let rec idk

    for i in 0..len do
      if Coefficients.[i] < Order then newCoeffs.[i] = Coefficients.[i]
      else
        status = false
        newCoeffs.[i] = Coefficients.[i] % Order

    let rec makeList (arr : 'a []) : 'a list =
      match arr with
        | [||] -> []
        | _    -> arr.[0] :: (makeList arr.[1..])

    (Chain(Simplices, (makeList newCoeffs), Dimension, Order, IsReduced), status)

  member this.reduce =
    let data      = reduceArr Simplices
    let newCoeffs =
      snd data
        |> arrMap (fun arr -> arrMap (fun i -> Coefficients.[i]))
          |> arrMap Array.sum
    Chain(fst data, newCoeffs, Dimension, Order, true)
*)

  static member getSimplexBoundary (simplex : int list) (simplices : int list list) index order (coeffs : int list) : int list list * int list =
    match simplex with
      | []        -> (simplices, coeffs)
      | (x :: xs) ->
        let s = simplex.[1..index] @ xs
        let c =
          if index % 2 = 0 then 1
          else
            if order = 2 then 0
            else order - 1
        Chain.getSimplexBoundary simplex (s :: simplices) (index + 1) order (c :: coeffs)
(*
  static member getSimplexBoundaries (dim : int) (order : int) (simplex : int []) : Chain =
    let newSimplices = Array.create dim
    let coeffs       = Array.create dim
    let l            = dim - 1

    for i in indices do
      let arr = Array.create l
      for j in 0..l do
        arr.[j]    = if j < i then simplex.[j]
                     else simplex.[j + 1]
      coeffs.[i]       = if i % 2 = 1 then order - 1 else 1
      newSimplices.[i] = arr

    Chain(newSimplices, coeffs, dim, order, false)

  member this.Boundaries : Chain [] =
    arrMap (getSimplexBoundaries Dimension Order) Simplices
*)

  static member organizeChains (chains : Chain list) : Matrix =

    let rec addAllElems (arg : 'a list) (res : 'a list) : 'a list =
      match arg with
        | []        -> res
        | (x :: xs) ->
          if List.exists (fun a -> a = x) res then addAllElems xs res
          else addAllElems xs (x :: res)

    let rec collect (arg : 'a list list) (result : 'a list) =
      match arg with
        | []        -> result
        | (x :: xs) -> collect xs (addAllElems x result)

    let allSimplices =
      chains
        |> List.map (fun (c : Chain) -> c.Simplices)
          |> fun a -> collect a []

    let rec getCoeff (i : int) (c : Chain) =
      match List.tryFindIndex (fun x -> x = allSimplices.[i]) c.Simplices with
        | None   -> 0
        | Some x -> c.Coefficients.[x]

    Matrix([|for chain in chains -> [|for i in 0..(allSimplices.Length - 1) -> getCoeff i chain|]|], chains.[0].Order)

type SimplicialComplex(OrderedSets : int list list) =
  member this.OrderedSets = OrderedSets

  member this.Dimension : int = maximum (List.map (fun (list : int list) -> list.Length : int) OrderedSets)
