module Matrix where

import Util
import Data.List

data Matrix a = Matrix [[a]] a

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order

getFstZeroCol :: Integral a => [[a]] -> Maybe Int
getFstZeroCol []     = Nothing
getFstZeroCol (r:rs) =
  case elemIndex 0 r of
    Nothing -> getFstZeroCol rs
    Just i  -> Just i

findPivot :: Integral a => Int -> Int -> [[a]] -> (Int, Maybe a)
findPivot row col []     = (row, Nothing)
findPivot row col (r:rs) = let elem = r !! col in
  if elem == 0 then findPivot (row + 1) col rs
  else (row, Just elem)

findNonDivisible :: Integral a => Int -> Int -> a -> [[a]] -> Maybe (Int, a)
findNonDivisible _ _ _ []             = Nothing
findNonDivisible row col pivot (r:rs) = let elem = r !! col in
  if elem `mod` pivot /= 0 then Just (row, elem)
  else findNonDivisible (row + 1) col pivot rs

improvePivot :: Integral a => Matrix a -> Matrix a
improvePivot mat = let elems     = getElems mat
                       col       = case getFstZeroCol elems of
                         Nothing -> length mat - 1
                         Just c  -> c
                       pivotData = findPivot 0 col elems
                       pivot     = case (snd pivotData) of
                         Nothing -> (mat !! col) !! (fst pivotData)
                         Just p  -> p
                       elem      = findNonDivisible 0 col pivot elems
                       gcdTriple = extEucAlg pivot elem
