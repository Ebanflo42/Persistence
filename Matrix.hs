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

{-
improvePivot :: Integral a => Matrix a -> Matrix a
improvePivot mat = let elems     = getElems mat --matrix elements
                       col       = case getFstZeroCol elems of --pivot column
                         Nothing -> length mat - 1
                         Just c  -> c
                       pivotData = findPivot 0 col elems
                       pivot     = case (snd pivotData) of  --pivot
                         Nothing -> (mat !! col) !! (fst pivotData)
                         Just p  -> p
                       elem      = case findNonDivisible 0 col pivot elems of --first non-divisible elements
                         Nothing -> 
                       gcdTriple = extEucAlg pivot elem --gcd + bezout coefficients
                       subMats   = getSubLists 0 
-}

improvePivot :: Integral a => Int -> (Int, a) -> (Int, a) -> Matrix a -> Matrix a
improvePivot col (pIndex, pivot) (index, elem) (Matrix elems ord) = 
  let gcdTriple = extEucAlg pivot elem --gcd + bezout coefficients
      gcd       = one gcdTriple
      q1        = pivot `div` gcd
      q2        = elem `div` gcd
      b         = pIndex < index
      subMats   = getSubLists 0 b (pIndex, index) elems
      subMat1   = two subMats
      subMat2   = thr subMats
      row1      = head subMat1
      row2      = head subMat2
      modulo    = map (\n -> n `mod` ord)
      newRow1   = if b then modulo (((two gcdTriple) `mul` row1) `add` ((thr gcdTriple) `mul` row2)))
                  else modulo (((two gcdTriple) `mul` row2) `add` ((thr gcdTriple) `mul` row1)))
      newRow2   = if b then modulo ((-q2 `mul` row1) `add` (q1 `mul` row2))
                  else modulo ((-q2 `mul` row2) `add` (q1 `mul` row1))
      newElems  = (one subMats) ++ (newRow1 : (tail subMat1)) ++ (newRow2 : (tail subMat2)) in
Matrix newElems ord
