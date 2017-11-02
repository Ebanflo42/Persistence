module Matrix

open Parser

let flip = fun (a, b) -> (b, a)

let scalMult (s : int) (vec : int []) : int [] =
  [|for x in vec -> s * x|]

let rec extEucAlg (a : int) (b : int) : int [] =
  let rec helper (r : int * int) (s : int * int) (t : int * int) =
    match snd r with
      | 0 -> [|fst r; fst s; fst t|]
      | _ ->
        let r1    = fst r
        let r2    = snd r
        let s2    = snd s
        let t2    = snd t
        let q     = r1 / r2
        let nextr = r1 - q * r2
        let nexts = fst s - q * s2
        let nextt = fst t - q * t2
        helper (r2, nextr) (s2, nexts) (t2, nextt)
  helper (a, b) (0, 1) (1, 0)

type Matrix(Elements : int [] [], Order : int) =
  member this.Elements = Elements
  member this.Order    = Order

  member this.printAttribs =
    print2dArr Elements
    printfn "%i" Order

  member this.isMatrix =
    Array.forall (fun (a : int []) -> a.Length = Elements.[0].Length) Elements.[1..]

  member this.Transpose =
    let elems =
      [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length) -> Elements.[j].[i]|]|]
    Matrix(elems, Order)

  member this.TransposeElems = [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length - 1) -> Elements.[j].[i]|]|]

  member this.findPivot : int * int = //first non-zero entry in the first column with a zero

    let rec getFstZeroRow (mat : int [] []) (index : int) =
      match mat with
        | [||] -> failwith "Couldn't find pivot"
        | _    ->
          if Array.exists (fun a -> a = 0) mat.[0] then index
          else getFstZeroRow mat.[1..] (index + 1)

    let rec find (mat : int [] []) =
      let i = getFstZeroRow mat 0
      match Array.tryFindIndex (fun a -> a <> 0) mat.[i] with
        | None   -> find mat.[(i + 1)..]
        | Some j -> (j, i)

    find this.TransposeElems

  member this.improvePivot =

    let indices   = this.findPivot //indices of pivot
    printfn "The indices of the pivot are %i and %i" (fst indices) (snd indices)
    let elem      = Elements.[fst indices].[snd indices] //pivot
    printfn "The pivot is %i" elem 
    let column    = [|for row in Elements -> row.[snd indices]|] //column containing pivot
    printfn "The column is"
    for x in column do printfn "%i " x

    match Array.tryFindIndex (fun a -> a % elem <> 0) column with //try to an entry not divisble by the pivot
      | None       -> this //if none, stop
      | Some index -> //otherwise get the index
        let elem2     = column.[index] //get the element at the index
        let gcdTriple = extEucAlg elem elem2 //get the gcd and the two coefficients that give the gcd when multiplying the numbers
        let gcd       = gcdTriple.[0] //get the gcd
        let q1        = elem/gcd //quotient of pivot by gcd
        let q2        = elem2/gcd //quotient of other entry by gcd

        let newElems =
          [|
            for i in 0..(Elements.Length - 1) -> 
              [|
                for j in 0..(Elements.[0].Length - 1) ->
                  if i = fst indices then 
                    (gcdTriple.[1]*Elements.[i].[j] + gcdTriple.[2]*Elements.[i].[snd indices]) % Order
                  elif i = index then 
                    (-q2*Elements.[i].[j] + q1*Elements.[i].[i]) % Order
                  else 
                    Elements.[i].[j]
                |]
              |]

        Matrix(newElems, Order).improvePivot

  member this.diagonalize =
    this.improvePivot.Transpose.improvePivot.Transpose