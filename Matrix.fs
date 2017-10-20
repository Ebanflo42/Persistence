module Matrix

type Matrix(Elements : int [] [], Order : int) =
  member this.Elements = Elements
  member this.Order    = Order

  member this.isMatrix =
    Array.forall (fun (a : 'a []) -> a.Length = Elements.[0].Length) Elements.[1..]
