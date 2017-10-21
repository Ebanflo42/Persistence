module Chain

open Matrix

type Chain(Simplices : int list list, Coefficients : int list, Dimension : int, Order : int) =
  member this.Simplices    = Simplices
  member this.Coefficients = Coefficients
  member this.Order        = Order
  //member this.IsReduced    = IsReduced

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

let rec getSimplexBoundary (simplex : int list) order (simplices : int list list) (coeffs : int list) index : Chain =
  match simplex with
    | []        -> Chain(simplices, coeffs, simplices.[0].Length - 1, order)//(simplices, coeffs)
    | (x :: xs) ->
      let s = simplex.[1..index] @ xs
      let c =
        if index % 2 = 0 then 1
        else
          if order = 2 then 0
          else order - 1
      getSimplexBoundary simplex order (s :: simplices) (c :: coeffs) (index + 1)

let rec organizeChains (chains : Chain list) : Matrix * int list list =
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

  (Matrix([|for chain in chains -> [|for i in 0..(allSimplices.Length - 1) -> getCoeff i chain|]|], chains.[0].Order), allSimplices)

let rec getBoundaryOperator (simplices : int list list) order : Matrix * int list list =
  let boundaries = List.map (fun (s : int list) -> getSimplexBoundary s order [] [] 0) simplices
  if List.forall (fun (c : Chain) -> c.VerifyDimension) boundaries then organizeChains boundaries
  else failwith "Somehow, the boundary of a simplex was found to have the wrong dimension"
