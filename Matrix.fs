module Matrix

type Matrix(Elements : int [] [], Order : int) =
  member this.Elements = Elements
  member this.Order    = Order

  member this.isMatrix =
    Array.forall (fun (a : 'a []) -> a.Length = Elements.[0].Length) Elements.[1..]

  member this.Transpose =
    [|for i in 0..(Elements.Length - 1) -> [|for j in 0..(Elements.[0].Length) -> Elements.[j].[i]|]|]

  member this.FindPivot =

    let rec getFstIndex (mat : int [] []) (index : int) =
      match mat with
        | [||] -> failwith "Couldn't find pivot"
        | _    ->
          if Array.exists (fun a -> a = 0) mat.[0] then index
          else getFstIndex mat.[1..] (index + 1)

    getFstIndex this.Transpose 0
(*
  static member improvePivot =

    let rec gcd a b =
      if b = 0
      then abs a
      else gcd b (a % b)
*)
