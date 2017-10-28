module Matrix

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

  member this.isMatrix =
    Array.forall (fun (a : int []) -> a.Length = Elements.[0].Length) Elements.[1..]

  member this.Transpose =
    let elems =
      [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length) -> Elements.[j].[i]|]|]
    Matrix(elems, Order)

  member this.TransposeElems = [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length - 1) -> Elements.[j].[i]|]|]

  member this.FindPivot : int * int =

    let rec getFstZeroRow (mat : int [] []) (index : int) =
      match mat with
        | [||] -> failwith "Couldn't find pivot 1"
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

    let indices   = this.FindPivot
    let elem      = Elements.[fst indices].[snd indices]
    let column    = [|for row in Elements -> row.[snd indices]|]
    match Array.tryFindIndex (fun a -> a % elem <> 0) column with
      | None       -> this
      | Some index ->
        let elem2     = column.[index]
        let gcdTriple = extEucAlg elem elem2
        let gcd       = gcdTriple.[0]
        let q1        = elem/gcd
        let q2        = elem2/gcd

        let newElems =
          [|
            for i in 0..(Elements.Length - 1) -> 
              [|
                for j in 0..(Elements.[0].Length - 1) ->
                  if i = fst indices then gcdTriple.[1]*Elements.[i].[j] + gcdTriple.[2]*Elements.[i].[snd indices]
                  elif i = index then -q2*Elements.[i].[j] + q1*Elements.[i].[i]
                  else Elements.[i].[j]
                |]
              |]

        Matrix(newElems, Order)
