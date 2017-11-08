module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] a

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order
--getNumColSwitches (Matrix _ _ switches) = switches

toString :: Matrix Int -> String 
toString matrix =
  let mat = flatten $ map (\row -> '\n':(flatten $ map (\e -> ' ':(show e)) row)) (getElems matrix) in
  mat ++ "\nmodulo " ++ (show $ getOrder matrix)

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

findFstNotAllZeroCol :: Integral a => Int -> [[a]] -> Maybe Int
findFstNotAllZeroCol _ []     = Nothing
findFstNotAllZeroCol i (r:rs) =
  if any (\n -> n /= 0) r then Just i
  else findFstNotAllZeroCol (i + 1) rs

--first arg is the row index, second arg is the first column with a zero entry
--finds the first non-zero element in the first column with a zero entry
--returns the row index and maybe the value at the index
{-
findPivot :: Integral a => Int -> Int -> [[a]] -> (Int, Maybe a)
findPivot row col []     = (row, Nothing)
findPivot row col (r:rs) = 
  let elem = r !! col in
  if elem == 0 then findPivot (row + 1) col rs
  else (row, Just elem)
-}

findPivot :: Integral a => Int -> [[a]] -> Maybe (Int, a)
findPivot _ []     = Nothing
findPivot n (r:rs) =
  let elem = head r in
  if elem /= 0 then Just (n, elem)
  else findPivot (n + 1) rs

--Finds all non-divisible elements in a column and their indices
findNonDivisible :: Integral a => Int -> a -> [a] -> [(Int, a)]
{--
findNonDivisible _ _ _ []             = []
findNonDivisible row col pivot (r:rs) =
  let rest = findNonDivisible (row + 1) col pivot rs in
    case par rest (r !! col) of
      0                      -> rest
      x | x == pivot         -> rest
      x | x `mod` pivot == 0 -> rest
      x                      -> (row, x) : rest
--}
findNonDivisible index pivot []     = []
findNonDivisible index pivot (x:xs) =
  case x of
    0                      -> findNonDivisible (index + 1) pivot xs
    a | a == pivot         -> findNonDivisible (index + 1) pivot xs
    a | a `mod` pivot == 0 -> findNonDivisible (index + 1) pivot xs
    _                      -> (index, x) : (findNonDivisible (index + 1) pivot xs)


splitMatrix :: Int -> ([[a]], [a], [[a]], [a], [[a]]) -> Int -> Int -> [[a]] -> ([[a]], [a], [[a]], [a], [[a]])
splitMatrix _ result _ _ [] = result
splitMatrix current (mat1, row1, mat2, row2, mat3) i j (r:rs)
  | j < i        = splitMatrix current (mat1, row1, mat2, row2, mat3) j i (r:rs)
  | current < i  = splitMatrix (current + 1) (r:mat1, row1, mat2, row2, mat3) i j rs
  | current == i = splitMatrix (current + 1) (mat1, r, mat2, row2, mat3) i j rs
  | current < j  = splitMatrix (current + 1) (mat1, row1, r:mat2, row2, mat3) i j rs
  | current == j = splitMatrix (current + 1) (mat1, row1, mat2, r, mat3) i j rs
  | otherwise    = splitMatrix (current + 1) (mat1, row1, mat2, row2, r:mat3) i j rs

--first argument is the index of the column of interest
--second arg is the row index of the pivot and the value of the pivot
--third arg is the other element's index and it's value
improvePivot :: Integral a => Int -> a -> [(Int, a)] -> Matrix a -> Matrix a
improvePivot pIndex pivot [(index, elem)] (Matrix elems ord) = 
  let gcdTriple = extEucAlg pivot elem --gcd + bezout coefficients
      gcd       = one gcdTriple
      q1        = pivot `div` gcd
      q2        = elem `div` gcd
      split     = splitMatrix 0 ([],[],[],[],[]) pIndex index elems
      row1      = five2 split
      row2      = five4 split

      modulo    = if ord == 0 then id
                  else (\n -> n `mod` ord)
{--
      newRow1   = modulo (((thr gcdTriple) `mul` row1) `add` ((two gcdTriple) `mul` row2))
      newRow2   = modulo (((-q2) `mul` row1) `add` (q1 `mul` row2))

      newElems  = (five1 split) ++ (newRow1 : (five3 split)) ++ (newRow2 : (five5 split))
      --}
      nonDivis  = findNonDivisible 0 col pivot elems in

  Matrix newElems ord

improvePivot col pair (nonDivis : rest) matrix =
  improvePivot col pair rest (improvePivot col pair [nonDivis] matrix)

--returns improved matrix and the index of the column to be eliminated
--TODO: fix column switching slowness
findAndImprovePivot :: Integral a => Matrix a -> (Int, Int, a, Matrix a)
findAndImprovePivot (Matrix elems ord x) =
  {--
  let column    = case findFstZeroCol elems of
                    Nothing -> 0
                    Just i  -> i
      pivotData = findPivot 0 column elems in
  
  case snd pivotData of
    Nothing -> findAndImprovePivot $ Matrix (switchCols column (length elems - 1 - x) elems) ord (x + 1)
    Just p  ->
      let pIndex = fst pivotData in
      case findNonDivisible pIndex column p (drop pIndex elems) of
        [] -> (column, pIndex, p, Matrix elems ord x)
        xs -> (column, pIndex, p, improvePivot column (pIndex, p) xs (Matrix elems ord x))
  --}
  case findPivot 0 elems of
    Nothing     -> findAndImprovePivot $ Matrix ((tail elems) ++ [head elems]) ord
    Just (n, e) ->


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
                              else
                                ((-e) `div` pivot) `mul` pRow `add` row)
                   (getElems mat) in
  Matrix (modulo newElems) (getOrder mat)

getMinimalEntries :: Integral a => Bool -> Matrix a -> Matrix a
getMinimalEntries b (Matrix elems order num) =
  let elim = eliminateEntries (Matrix elems order num) in
  case getElems elim of
    m | m == elems ->
      let tr = transpose elems in
      case eliminateEntries (Matrix tr order num) of
        Matrix tr o n -> if b then Matrix (transpose tr) o n else Matrix tr o n
        m             -> getMinimalEntries True m
    _               -> getMinimalEntries b elim