module Util

let bind (o : Option<'a>) (f : 'a -> Option<'b>) : Option<'b> =
  match o with
    | None   -> None
    | Some x -> f x

let rec checkSuperSet (set : int list) (l1 : int) (Set : int list) (l2 : int) : bool =
  if l2 < l1 then false elif List.take l1 Set = set then true else checkSuperSet set l1 (Set.[l1..]) (l2 - l1)

let rec checkSubSet (Set : int list) (l1 : int) (set : int list) (l2 : int) : bool =
  checkSuperSet set l2 Set l1

let rec maximum (arr : int []) : int = Array.fold max arr.[0] arr.[1..]

let arrMap (f : 'a -> 'b) (arr : 'a []) : 'b [] = [|for x in arr -> f x|]

let toResizeArray (l : 'a list) =
  let result = new ResizeArray<'a>()
  for elem in l do
    result.Add(elem)
  result

let rec drop (n : int) (list : 'a list) : 'a list =
  match list with
    | [] -> []
    | _  -> if n > 0 then drop (n - 1) (List.tail list) else list

let removeAt (n : int) (list : 'a list) : 'a list = (List.take (n - 1) list) @ (drop n list)

let rec findIndices (arr : 'a []) (elem : 'a) (index : int) : int list =
  if arr = [||] then []
  elif arr.[0] = elem then index :: (findIndices arr.[1..] elem (index + 1))
  else findIndices arr.[1..] elem (index + 1)

let rec findAndInsert (i : int) (elem : int) (list : int list list) : int list list =
  list.[0..(i - 1)] @ ((elem :: list.[i]) :: list.[(i + 1)..])
  //[|for n in 0..list.Length - 1 -> if n = i then elem :: list.[n] else list.[n]|] //Arrangement could confuse compiler

(*
let rec reduceArr (input : 'a []) : 'a [] * int [] [] =

  let rec makeSoln (data : int list) (ans : int list) (indices : int list list) (index : int) : int list * int list list =
    match data with
      | []         -> (ans, indices)
      | (x :: xs)  ->
        match List.tryFindIndex x ans with
          | None   -> makeSoln xs (x :: ans) indices (index + 1)
          | Some i ->
            makeSoln xs ans a (index + 1) (findAndReplace i index indices)

  let rec makeSoln2 (data : int []) (ans : int []) (indices : int [] []) : int [] * int [] [] =
    match data with
      | [||] -> (ans, indices)
      | _    ->
        match Array.tryFindIndex x ans with
          | None   -> makeSoln2 xs (Array.append x ans) indices (index + 1)
          | Some i ->
            makeSoln2

  let soln   = makeSoln [|for elem in input -> elem|] [] [] 0
  let len    = soln.Length - 1
  let result = (Arrary.create len, Arrary.create len)
  for i in 0..len do
    (fst result).[i] = (fst soln).[i]
    let s            = (snd soln).[i]
    let len1         = s.Length - 1
    let arr          = Array.create len1
    for j in 0..len1 do
      arr.[j] = s.[j]
    (snd result).[i] = arr
  result
*)
let rec makeSoln (data : int list) (ans : int list) (indices : int list list) (index : int) : int list * int list list =
  match data with
    | []         -> (ans, indices)
    | (x :: xs)  ->
      match List.tryFindIndex (fun a -> a = x) ans with
        | None   -> makeSoln xs (x :: ans) indices (index + 1)
        | Some i ->
          makeSoln xs ans (findAndInsert i index indices) (index + 1)


let rec factorial (n : int) =
  match n with
    | 0 -> 1
    | n -> n * factorial (n - 1)

let rec binomialCoeff (n : int) (k : int) : int =
  let rec ordChoice (i : int) (j : int) : int =
    match j with
      | 0 -> 1
      | 1 -> i
      | 2 -> i*(i - 1)
      | 3 -> i*(i - 1)*(i - 2)
      | _ -> (i - j + 1)*ordChoice i (j - 1)
  (ordChoice n k)/(factorial k)

let rec getSubLists (len : int) (list : int list) : int list [] =
  let helper = []
  let ssLen = list.Length
  let result = [|for i in 1..(binomialCoeff ssLen len) -> helper|]

  let rec findSubLists (current : int list) (guess : int) (b : bool) (iter : int) : unit =
    if b && current.Length = len then result.[iter] = current; findSubLists current guess false (iter + 1)
    elif guess < ssLen then
      let x = list.[guess]
      findSubLists (x :: current) (guess + 1) true iter
      findSubLists (removeAt x current) (guess + 1) true iter
    else ()

  findSubLists helper 0 true 0
  result
