module Matrix where

import Util
import Data.List

data Matrix a = Matrix [[a]] a

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order

--switch two columns, second must be greater than first
switchCols :: Int -> Int -> [[a]] -> [[a]]
switchCols col1 col2 matrix =
  let separate1 = map (splitAt col1) matrix
      separate2 = map (splitAt (col2 - col1)) (snd separate1) in
  (fst separate1) ++ (((head . snd) separate2) : ((tail . fst) separate2)) ++ (((head . fst) separate2) : ((tail . snd) separate2))

--finds the first column with a zero entry
getFstZeroCol :: Integral a => [[a]] -> Maybe Int
getFstZeroCol []     = Nothing
getFstZeroCol (r:rs) =
  case elemIndex 0 r of
    Nothing -> getFstZeroCol rs
    Just i  -> Just i

--first arg is the row index, second arg is the first column with a zero entry
--finds the first non-zero element in the first column with a zero entry
--returns the row index and maybe the value at the index
findPivot :: Integral a => Int -> Int -> [[a]] -> (Int, Maybe a)
findPivot row col []     = (row, Nothing)
findPivot row col (r:rs) = let elem = r !! col in
  if elem == 0 then findPivot (row + 1) col rs
  else (row, Just elem)

--maybe finds the first element in a column that isn't divisible by the pivot
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

--first argument is the index of the column of interest
--second arg is the row index of the pivot and the value of the pivot
--third arg is the other element's index and it's value
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
      newRow1   = if b then modulo (((two gcdTriple) `mul` row1) `add` ((thr gcdTriple) `mul` row2))
                  else modulo (((two gcdTriple) `mul` row2) `add` ((thr gcdTriple) `mul` row1))
      newRow2   = if b then modulo (((-q2) `mul` row1) `add` (q1 `mul` row2))
                  else modulo (((-q2) `mul` row2) `add` (q1 `mul` row1))
      newElems  = (one subMats) ++ (newRow1 : (tail subMat1)) ++ (newRow2 : (tail subMat2)) in
  Matrix newElems ord

--returns improved matrix and the index of the column to be eliminated
findAndImprovePivot :: Integral a => Matrix a -> (Int, Matrix a)
findAndImprovePivot (Matrix elems ord) =
  let column    = case getFstZeroCol elems of
    Nothing -> length elems - 1
    Just i  -> i
      pivotData = findPivot 0 column elems in
  case snd pivotData of
    Nothing -> findAndImprovePivot $ Matrix (switchCols column (length elems - 1)) ord
    Just p  ->
      case findNonDivisible 0 column p elems of
        Nothing -> (column, Matrix elems ord)
        Just x  -> (column, improvePivot column (fst pivotData, p) x (Matrix elems ord))