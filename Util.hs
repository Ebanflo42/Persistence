module Util where

import Data.List

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c

exists :: Eq a => a -> [a] -> Bool
exists elem list =
  case list of
    []       -> False
    (x : xs) ->
      if x == elem then True
      else exists elem xs

--extended Euclidean algorithm
helper :: Integral a => (a, a) -> (a, a) -> (a, a) -> (a, a, a)
helper r s t =
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
      helper (r2, nextr) (s2, nexts) (t2, nextt)

extEucAlg :: Integral a => a -> a -> (a, a, a)
extEucAlg a b = helper (a, b) (0, 1) (1, 0)

--elimates duplicates in first argument, second argument is the result
regroupElems :: Eq a => [[a]] -> [[a]] -> [[a]]
regroupElems arg res =
    case arg of
        []       -> res
        (x : xs) ->
          if exists x res then regroupElems xs res
          else regroupElems xs (x : res)

collect :: Eq a => [[[a]]] -> [[a]] -> [[a]]
collect arg result =
  case arg of
    []       -> result
    (x : xs) -> collect xs (regroupElems x result)

forall :: (a -> Bool) -> [a] -> Bool
forall _ []     = True
forall p (x:xs) = if p x then forall p xs
                  else False