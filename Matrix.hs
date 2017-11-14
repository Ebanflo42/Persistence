module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] a Int Int

getElems (Matrix elems _ _ _) = elems --elements of the matrix
getOrder (Matrix _ order _ _) = order --modulus of the elements
getIndex (Matrix _ _ index _) = index --index of the current pivot
getMaxIndex (Matrix _ _ _ m)  = m

initializeMatrix :: Integral a => a -> [[a]] -> Matrix a
initializeMatrix order elems =
  Matrix elems order 0 ((min (length $ elems !! 0) (length elems)) - 1)

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

--looks for the first non-zero diagonal entry
findPivot :: Integral a => Int -> [[a]] -> Maybe (Int, a)
findPivot _ []     = Nothing
findPivot n (r:rs) =
  let elem = r !! n in
  if elem /= 0 then Just (n, elem)
  else findPivot (n + 1) rs

--Finds all non-divisible elements in a row and their indices
--first arg is current index, second is the element for which divisibility is being checked
findNonDivisible :: Integral a => Int -> a -> [a] -> [(Int, a)]
findNonDivisible index pivot []     = []
findNonDivisible index pivot (x:xs) =
  case x of
    0                      -> findNonDivisible (index + 1) pivot xs
    a | a == pivot         -> findNonDivisible (index + 1) pivot xs
    a | a `mod` pivot == 0 -> findNonDivisible (index + 1) pivot xs
    _                      -> (index, x) : (findNonDivisible (index + 1) pivot xs)

--Given two indices, breaks up a matrix into everthing before the first index,
--the row at the first index, everything between the first and second indices,
--the row at the second index, and everthing after that
splitMatrix :: Int -> ([[a]], [a], [[a]], [a], [[a]]) -> Int -> Int -> [[a]] -> ([[a]], [a], [[a]], [a], [[a]])
splitMatrix _ result _ _ [] = result
splitMatrix current (mat1, row1, mat2, row2, mat3) i j (r:rs)
  | j < i        = splitMatrix current (mat1, row1, mat2, row2, mat3) j i (r:rs)
  | current < i  = splitMatrix (current + 1) (r:mat1, row1, mat2, row2, mat3) i j rs
  | current == i = splitMatrix (current + 1) (mat1, r, mat2, row2, mat3) i j rs
  | current < j  = splitMatrix (current + 1) (mat1, row1, r:mat2, row2, mat3) i j rs
  | current == j = splitMatrix (current + 1) (mat1, row1, mat2, r, mat3) i j rs
  | otherwise    = splitMatrix (current + 1) (mat1, row1, mat2, row2, r:mat3) i j rs

--first two args are the indices of the rows undergoing the operation
--3rd and 4th are the coefficients multiplying the two rows to give the row at the pivot index
rowOperation :: Integral a => Int -> Int -> a -> a -> a -> a -> Matrix a -> [[a]]
rowOperation pIndex index pcoeff1 pcoeff2 coeff1 coeff2 (Matrix elems _ _ _)
  | pIndex == index     = error "Tried to perform a row operation on a single row"
  | pIndex < index      =
    let first  = take pIndex elems
        second = drop (pIndex + 1) (take index elems)
        third  = drop (index + 1) elems
        row1   = elems !! pIndex
        row2   = elems !! index in
    first ++ (((pcoeff1 `mul` row1) `add` (pcoeff2 `mul` row2)):second) ++ (((coeff1 `mul` row1) `add` (coeff2 `mul` row2)):third)
  | otherwise           =
    let first  = take index elems
        second = drop (index + 1) (take pIndex elems)
        third  = drop (pIndex + 1) elems
        row1   = elems !! index
        row2   = elems !! pIndex in
    first ++ (((coeff1 `mul` row1) `add` (coeff2 `mul` row2)):second) ++ (((pcoeff1 `mul` row1) `add` (pcoeff2 `mul` row2)):third)

{--}
findRow :: Integral a => Int -> Int -> [[a]] -> Maybe (Either Int (Int, Int))
findRow i max [r]     =
  if i >= max then Nothing
  else if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else Just $ Left i
findRow i max (r:rs) =
  if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> findRow (i + 1) max rs
      Just x  -> Just $ Right (i, x)
  else Just $ Left i

choosePivot :: Integral a => Matrix a -> (Maybe a, Matrix a)
choosePivot (Matrix elems ord i max) =
  case findRow i max (drop i elems) of
    Nothing             -> (Nothing, Matrix elems ord i max)
    Just (Left x)       -> (Just $ elems !! x !! x, Matrix elems ord x max)
    Just (Right (x, y)) ->
      let pivot = elems !! x !! y
          newElems = map (switchElems x y) elems in
      (Just pivot, Matrix newElems ord x max)

colOperationHelper :: Integral a => Int -> ((a, a, a, a, a), Int) -> [a] -> [a]
colOperationHelper pIndex ((gcd, coeff1, coeff2, q1, q2), index) row
  | index == pIndex = error "Tried to do column operation on a single column."
  | index < pIndex  =
    let first  = take index row
        second = drop (index + 1) (take pIndex row)
        third  = drop (pIndex + 1) row
        elem1  = row !! index
        elem2  = row !! pIndex in
    first ++ (((-q2)*elem1 + q1*elem2) : second) ++ ((coeff1*elem2 + coeff2*elem1) : third)
  | otherwise       =
    let first  = take pIndex row
        second = drop (pIndex + 1) (take index row)
        third  = drop (index + 1) row
        elem1  = row !! index
        elem2  = row !! pIndex in
    first ++ ((coeff1*elem2 + coeff2*elem1) : second) ++ (((-q2)*elem1 + q1*elem2) : third)

improvePivot :: Integral a => (a, Matrix a) -> Matrix a
improvePivot (pivot, Matrix elems ord pIndex max) =
  let row          = elems !! pIndex in
  case indexAndElem (\n -> n `mod` pivot /= 0) row of
    Nothing     -> Matrix elems ord pIndex max
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n
          gcd          = one gcdTriple
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i) --may need to switch order
          newElems     = map (colOperationHelper pIndex transform) elems in
      improvePivot (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

eliminateEntries :: Integral a => Matrix a -> Matrix a
eliminateEntries (Matrix elems ord pIndex max) =
  let row      = elems !! pIndex
      pivot    = row !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  Matrix newElems ord pIndex max

getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm matrix =
  case choosePivot matrix of
    (Nothing, _)  -> matrix
    (Just p, mat) ->
      if p == 0 then error "Pivot was found to be zero."
      else
        let m      = improvePivot (p, mat)
            elim1  = eliminateEntries m
            pIndex = getIndex elim1
            ord    = getOrder elim1
            elems  = getElems elim1
            tr     = Matrix (transpose $ getElems elim1) ord pIndex (getMaxIndex elim1)
            elim2  = eliminateEntries $ improvePivot (p, tr) in
        getSmithNormalForm elim2
--}