module Util where

import Data.List

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c

not1 (_, b, c) = (b, c)
not2 (a, _, c) = (a, c)
not3 (a, b, _) = (a, b)

mul :: Num a => a -> [a] -> [a]
mul s vec = map (*s) vec

add :: Num a => [a] -> [a] -> [a]
add [] _ = error "First list wasn't long enough"
add _ [] = error "Second list wasn't long enough"
add (x:xs) (y:ys) = (x + y) : (xs `add` ys)

--first argument is current index, third argument is the two indices to split at, second argument is which index is greater,
--fourth arg is the list to be split
getSubLists :: Int -> Bool -> (Int, Int) -> [a] -> ([a], [a], [a])
getSubLists _ _ _ []          = ([], [], [])
getSubLists i t (a, b) (x:xs) =
  if t then
    if i < a then let sublists = getSubLists (i + 1) t (a, b) xs in
      (x : (one sublists), two sublists, thr sublists)
    else if i < b then let sublists = getSubLists (i + 1) t (a, b) xs in
      (one sublists, x : (two sublists), thr sublists)
    else let sublists = getSubLists (i + 1) t (a, b) xs in
      (one sublists, two sublists, x : (thr sublists))
  else
    if i < b then let sublists = getSubLists (i + 1) t (a, b) xs in
      (x : (one sublists), two sublists, thr sublists)
    else if i < a then let sublists = getSubLists (i + 1) t (a, b) xs in
      (one sublists, x : (two sublists), thr sublists)
    else let sublists = getSubLists (i + 1) t (a, b) xs in
      (one sublists, two sublists, x : (thr sublists))

switchElems :: Int -> Int -> [a] -> [a]
switchElems i j list =
  let sublists = getSubLists 0 True (i, j) list in
    (one sublists) ++ ((head (thr sublists)) : (tail (two sublists))) ++ ((head (two sublists)) : (tail (thr sublists)))

exists :: Eq a => a -> [a] -> Bool
exists elem list =
  case list of
    []       -> False
    (x : xs) ->
      if x == elem then True
      else exists elem xs

--extended Euclidean algorithm
eeaHelper :: Integral a => (a, a) -> (a, a) -> (a, a) -> (a, a, a)
eeaHelper r s t =
  case snd r of
    0 -> (fst r, fst s, fst t)
    _ ->
      let r1    = fst r
          r2    = snd r
          s2    = snd s
          t2    = snd t
          q     = r1 `div` r2
          nextr = r1 - q*r2
          nexts = fst s - q*s2
          nextt = fst t - q*t2 in
      eeaHelper (r2, nextr) (s2, nexts) (t2, nextt)

extEucAlg :: Integral a => a -> a -> (a, a, a)
extEucAlg a b = eeaHelper (a, b) (0, 1) (1, 0)

--elimates duplicates in first argument, second argument is the result
regroupElems :: Eq a => [[a]] -> [[a]] -> [[a]]
regroupElems arg res =
    case arg of
        []       -> res
        (x : xs) ->
          if exists x res then regroupElems xs res
          else regroupElems xs (x : res)

chelper :: Eq a => [[a]] -> [[[a]]] -> [[a]]
chelper result arg =
  case arg of
    []       -> result
    (x : xs) -> chelper (regroupElems x result) xs

collect :: Eq a => [[[a]]] -> [[a]]    
collect = chelper []

forall :: (a -> Bool) -> [a] -> Bool
forall _ []     = True
forall p (x:xs) = if p x then forall p xs
                  else False