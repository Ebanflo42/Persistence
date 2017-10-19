open util.fs
open parser.fs

type HasseNode(Vertices : int [], Parents : HasseNode [], Children : HasseNode []) =
  member this.Vertices = Vertices
  member this.Parents  = Parents
  member this.Children = Children

  member this.checkNodeChilds =
    let dim = Vertices.Length
    if dim = 1 && Children = [||] then true
    else Children
      |> arrMap (fun nodes -> nodes.Vertices.Length)
        |> Array.forall (fun x -> x = dim - 1)

  member this.checkNodeParens =
    if Parents = [||] then true
    else Parents
      |> arrMap (fun nodes -> nodes.Vertices.Length)
        |> Array.forall (fun x -> x = Vertices.Length + 1)

  member this.checkNode =
    this.checkNodeChilds && this.checkNodeParens

type SimplicialComplex(OrderedSets : int [] []) =
  member this.OrderedSets = OrderedSets

  member this.Dimension : int = maximum (arrMap (fun (arr : int []) -> arr.Length : int) OrderedSets)

type Chain(Simplices : int [] [], Coefficients : int [], Dimension : int, Order : int, IsReduced : bool) =
  member this.Simplices    = Simplices
  member this.Coefficients = Coefficients
  member this.Order        = Order
  member this.IsReduced    = IsReduced

  member this.VerifyDimension : bool =
    Simplices.Length = Coefficients.Length && Array.forall (fun (arr : int []) -> arr.Length = Dimension) Simplices

  member this.VerifyOrder : Chain * bool =
    let mutable status : bool = true
    let len = Coefficients.Length - 1
    let newCoeffs = Array.create len

    for i in 0..len do
      if Coefficients.[i] < Order then newCoeffs.[i] = Coefficients.[i]
      else
        status = false
        newCoeffs.[i] = Coefficients.[i] % Order

    (Chain(Simplices, newCoeffs, Dimension, Order, IsReduced), status)

  member this.reduce =
    let data      = reduceArr Simplices
    let newCoeffs =
      snd data
        |> arrMap (fun arr -> arrMap (fun i -> Coefficients.[i]))
          |> arrMap Array.sum
    Chain(fst data, newCoeffs, Dimension, Order, true)

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

//TESTING
let _ = while true do
          let input = System.Console.ReadLine()
          let intArr = input |> parse2DIntArr |> finishParsing
          if intArr = [||] then printfn "Please give valid input" else printActualArr intArr
