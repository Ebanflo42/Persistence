module Matrix where

import Util
import Data.List

data Matrix a = Matrix [[a]] a

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order

getFstZeroCol :: Integral a => [[a]] -> Int
getFstZeroCol []     = error "Couldn't find pivot 1"
getFstZeroRow (r:rs) =
  case elemIndex 0 r of
    Nothing -> getFstZeroRow rs
    Just i  -> i

findPivot :: Integral a => Int -> [[a]] -> (Int, Int)
findPivot _ []     = (-1, -1)
findPivot i (r:rs) = if r !! i /= 0 then (0, i)
  else (fst (findPivot i rs) + 1, i)