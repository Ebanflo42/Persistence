module Matrix

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
    Array.forall (fun (a : 'a []) -> a.Length = Elements.[0].Length) Elements.[1..]

  member this.Transpose =
    let elems =
      [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length) -> Elements.[j].[i]|]|]
    Matrix(elems, Order)
(*
  member this.FindPivot : int * int =

    let rec getFstIndex (mat : int [] []) (index : int) =
      match mat with
        | [||] -> failwith "Couldn't find pivot"
        | _    ->
          match Array.tryFindIndex (fun x -> x = 0) mat.[0] with
            | None   -> getFstIndex mat.[1..] (index + 1)
            | Just i -> (index, i)

    getFstIndex this.Elements 0

  member this.improvePivot =

    let rec gcd a b =
      if b = 0
      then abs a
      else gcd b (a % b)

    let indices = this.FindPivot
    let row     = Elements.[fst indices]
    let elem    = row.[snd indices]

    let rec findGCD (arr : int []) =
      match arr with
      | []        -> failwith "Couldn't find Smith normal form"
      | (x :: xs) ->
        if x % elem != 0 then gcd x elem
        else findGCD xs
*)
