module Util where

import Data.List
import Control.Parallel

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c

not1 (_, b, c) = (b, c)
not2 (a, _, c) = (a, c)
not3 (a, b, _) = (a, b)

five1 (a, _, _, _, _) = a
five2 (_, b, _, _, _) = b
five3 (_, _, c, _, _) = c
five4 (_, _, _, d, _) = d
five5 (_, _, _, _, e) = e

four1 (a, _, _, _) = a
four2 (_, b, _, _) = b
four3 (_, _, c, _) = c
four4 (_, _, _, d) = d

mul :: Num a => a -> [a] -> [a]
mul s = map (*s)

add :: Num a => [a] -> [a] -> [a]
add [] []         = []
add [] y          = y
add x []          = x
add (x:xs) (y:ys) = (x + y) : (xs `add` ys)

--first argument is current index, third argument is the two indices to split at, second argument is which index is greater,
--fourth arg is the list to be split
getSubLists :: Int -> (Int, Int) -> [a] -> ([a], [a], [a])
getSubLists _ _ []          = ([], [], [])
getSubLists i (a, b) (x:xs) =
  if i < a then let sublists = getSubLists (i + 1) (a, b) xs in
    (x : (one sublists), two sublists, thr sublists)
  else if i < b then let sublists = getSubLists (i + 1) (a, b) xs in
    (one sublists, x : (two sublists), thr sublists)
  else let sublists = getSubLists (i + 1) (a, b) xs in
    (one sublists, two sublists, x : (thr sublists))

splitListHelper :: Int -> ([a], Maybe a, [a], Maybe a, [a]) -> Int -> Int -> [a] -> ([a], Maybe a, [a], Maybe a, [a])
splitListHelper _ result _ _ []  = result
splitListHelper current (l1, e1, l2, e2, l3) i j (x:xs)
  | current < i  = splitListHelper (current + 1) (x:l1, e1, l2, e2, l3) i j xs
  | current == i = splitListHelper (current + 1) (l1, Just x, l2, e2, l3) i j xs
  | current < j  = splitListHelper (current + 1) (l1, e1, x:l2, e2, l3) i j xs
  | current == j = splitListHelper (current + 1) (l1, e1, l2, Just x, l3) i j xs
  | otherwise    = splitListHelper (current + 1) (l1, e1, l2, e2, x:l3) i j xs

switchElems :: Int -> Int -> [a] -> [a]
switchElems i j list =
  if i == j then list
  else let fstTwo = splitAt i list
           sndTwo = splitAt (j - i) (snd fstTwo) in
       (fst fstTwo) ++ ((head $ snd sndTwo) : (tail $ fst sndTwo)) ++ ((head $ fst sndTwo) : (tail $ snd sndTwo))
  {-
  let sublists = getSubLists 0 (i, j) list in
    (one sublists) ++ ((head (thr sublists)) : (tail (two sublists))) ++ ((head (two sublists)) : (tail (thr sublists)))
    -}

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
      eeaHelper (par (s2, nexts) (r2, nextr)) (par (t2, nextt) (s2, nexts)) (t2, nextt)

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
forall p (x:xs) = (p x) && (forall p xs)

exists :: Eq a => a -> [a] -> Bool
exists elem list =
  case list of
    []       -> False
    (x : xs) -> (x == elem) || (exists elem xs)