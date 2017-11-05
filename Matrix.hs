module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] a deriving Show

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order

--switch two columns, second must be greater than first
--switchElems found in Util
switchCols :: Int -> Int -> [[a]] -> [[a]]
switchCols col1 col2 = (transpose . (switchElems col1 col2) . transpose)

--finds the first column with a zero entry
findFstZeroCol :: Integral a => [[a]] -> Maybe Int
findFstZeroCol []     = Nothing
findFstZeroCol (r:rs) =
  let rest  = findFstZeroCol rs
      index = elemIndex 0 r in
  case seq index (par rest index) of
    Nothing -> findFstZeroCol rs
    Just i  -> Just i

--first arg is the row index, second arg is the first column with a zero entry
--finds the first non-zero element in the first column with a zero entry
--returns the row index and maybe the value at the index
findPivot :: Integral a => Int -> Int -> [[a]] -> (Int, Maybe a)
findPivot row col []     = (row, Nothing)
findPivot row col (r:rs) = 
  let elem = r !! col in
  if elem == 0 then findPivot (row + 1) col rs
  else (row, Just elem)

--maybe finds the first element in a column that isn't divisible by the pivot
findNonDivisible :: Integral a => Int -> Int -> a -> [[a]] -> [(Int, a)]
findNonDivisible _ _ _ []             = []
findNonDivisible row col pivot (r:rs) =
  let elem = r !! col
      rest = findNonDivisible (row + 1) col pivot rs
      bool = elem `mod` pivot /= 0 in
  if seq bool (par rest bool) then (row, elem) : rest
  else rest

splitMatrix :: Int -> ([[a]], [a], [[a]], [a], [[a]]) -> Int -> Int -> [[a]] -> ([[a]], [a], [[a]], [a], [[a]])
splitMatrix _ result _ _ [] = result
splitMatrix current (mat1, row1, mat2, row2, mat3) i j (r:rs)
  | current < i  = splitMatrix (current + 1) (r:mat1, row1, mat2, row2, mat3) i j rs
  | current == i = splitMatrix (current + 1) (mat1, r, mat2, row2, mat3) i j rs
  | current < j  = splitMatrix (current + 1) (mat1, row1, r:mat2, row2, mat3) i j rs
  | current == j = splitMatrix (current + 1) (mat1, row1, mat2, r, mat3) i j rs
  | otherwise    = splitMatrix (current + 1) (mat1, row1, mat2, row2, r:mat3) i j rs

--first argument is the index of the column of interest
--second arg is the row index of the pivot and the value of the pivot
--third arg is the other element's index and it's value
improvePivot :: Integral a => Int -> (Int, a) -> [(Int, a)] -> Matrix a -> Matrix a
improvePivot col (pIndex, pivot) [(index, elem)] (Matrix elems ord) = 
  let gcdTriple = extEucAlg pivot elem --gcd + bezout coefficients
      --TODO: bezout coefficients might be inthe wrong order
      gcd       = one gcdTriple
      q1        = pivot `div` gcd
      q2        = elem `div` gcd
      {-
      subMats   = getSubLists 0 b (pIndex, index) elems
      subMat1   = two subMats
      subMat2   = thr subMats
      row1      = head subMat1
      row2      = head subMat2
      -}
      split     = splitMatrix 0 ([],[],[],[],[]) pIndex index elems
      row1      = five2 split
      row2      = five4 split

      modulo    = if ord == 0 then id
                  else map (\n -> n `mod` ord)

      newRow1   = modulo (((two gcdTriple) `mul` row1) `add` ((thr gcdTriple) `mul` row2))
      newRow2   = modulo (((-q2) `mul` row1) `add` (q1 `mul` row2))

      newElems  = (five1 split) ++ (newRow1 : (five3 split)) ++ (newRow2 : (five5 split))
      nonDivis  = findNonDivisible 0 col pivot elems in

  Matrix newElems ord

improvePivot col pair (nonDivis : rest) matrix =
  improvePivot col pair rest (improvePivot col pair [nonDivis] matrix)

--returns improved matrix and the index of the column to be eliminated
--TODO: fix column switching slowness
findAndImprovePivot :: Integral a => Matrix a -> (Int, Int, a, Matrix a)
findAndImprovePivot (Matrix elems ord) =
  let column    = case findFstZeroCol elems of
                    Nothing -> length elems - 1
                    Just i  -> i
      pivotData = findPivot 0 column elems in
  case snd pivotData of
    Nothing -> findAndImprovePivot $ Matrix (switchCols column (length elems - 1) elems) ord
    Just p  ->
      let pIndex = fst pivotData in
      case findNonDivisible pIndex column p (drop pIndex elems) of
        [] -> (column, pIndex, p, Matrix elems ord)
        x  -> (column, pIndex, p, improvePivot column (pIndex, p) x (Matrix elems ord))

eliminateEntries :: Integral a => Matrix a -> Matrix a
eliminateEntries matrix = 
  let improved = findAndImprovePivot matrix
      col      = four1 improved
      mat      = four4 improved
      elems    = getElems mat
      pRow     = elems !! (four2 improved)
      pivot    = four3 improved
      ord      = getOrder mat
      modulo   = if ord == 0 then id
                 else (map . map) (\n -> n `mod` ord)
      newElems = map (\row -> let e = row !! col in
                              if e == pivot then row
                              else let q = e `div` pivot in
                                (-q) `mul` pRow `add` row)
                   (getElems mat) in
  Matrix (modulo newElems) (getOrder mat)