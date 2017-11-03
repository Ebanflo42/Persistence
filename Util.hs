module Util where

import Data.List

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c

not1 (_, b, c) = (b, c)
not2 (a, _, c) = (a, c)
not3 (a, b, _) = (a, b)

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