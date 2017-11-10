module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] a Int Int

getElems (Matrix elems _ _ _) = elems --elements of the matrix
getOrder (Matrix _ order _ _) = order --modulus of the elements
getLength (Matrix _ _ len _)  = len   --the minimum of the column and row length of the matrix minus one (maximum val for index)
getIndex (Matrix _ _ _ index) = index --index of the current pivot

initializeMatrix :: Integral a => a -> [[a]] -> Matrix a
initializeMatrix order []    = error "Nobody likes an empty matrix"
initializeMatrix order elems = Matrix elems order (min (length $ head elems) (length elems) - 1) 0

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

--first argument is the index of the column of interest
--second arg is the row index of the pivot and the value of the pivot
--third arg is the other element's index and it's value
{--
improvePivot :: Integral a => Int -> a -> [(Int, a)] -> Matrix a -> Matrix a
improvePivot _ _ [] m                                            = m
improvePivot pIndex pivot [(index, elem)] (Matrix elems ord l n) =
  let gcdTriple = extEucAlg pivot elem --gcd + bezout coefficients
      gcd       = one gcdTriple
      q1        = pivot `div` gcd
      q2        = elem `div` gcd
      newElems  = rowOperation pIndex index (thr gcdTriple) (two gcdTriple) (-q2) q1 (Matrix elems ord l n) in

  Matrix newElems ord l n

improvePivot col pair (nonDivis : rest) matrix =
  improvePivot col pair rest (improvePivot col pair [nonDivis] matrix)
--}
--returns index of column to be eliminated, pivot, and the improved matrix
{--
findAndImprovePivot :: Integral a => Matrix a -> (Int, a, Matrix a)
findAndImprovePivot (Matrix elems ord l num) =
  case findPivot num (drop num elems) of
    Nothing     -> error "Couldn't find pivot"
      {--
      case num of
        n | n == length elems -> (0, 0, Matrix elems ord num)
        n                     -> findAndImprovePivot $ Matrix ((tail elems) ++ [head elems]) ord (n + 1)
      --}
    Just (i, p) ->
      (i, p, improvePivot i p (findNonDivisible 0 p (elems !! i)) (Matrix elems ord l i))
--}
--calls findAndImprovePivot then eliminates the appropriate column
{--
eliminateEntries :: Integral a => Matrix a -> Matrix a
eliminateEntries matrix = 
  let improved = findAndImprovePivot matrix
      pIndex   = one improved
      mat      = thr improved
      elems    = getElems mat
      pRow     = elems !! pIndex
      pivot    = two improved
      ord      = getOrder mat
      newElems = map (\row -> let e = row !! pIndex in
                              if e == pivot || e == 0 then row
                              else
                                (((-e) `div` pivot) `mul` pRow) `add` row)
                   (getElems mat) in
  Matrix newElems (getOrder mat) (getLength mat) (getIndex mat)
--}
{--
minimizeEntries :: Integral a => Matrix a -> Matrix a
minimizeEntries (Matrix elems ord len n) =
  let elim = eliminateEntries (Matrix elems ord len n)
      tr   = eliminateEntries $ Matrix (transpose $ getElems elim) ord len (getIndex elim) in
  case getIndex tr of
    i | i == len -> Matrix (((if ord == 0 then id else (map . map) (\n -> n `mod` ord)) . transpose . getElems) tr) ord len i
    i            -> minimizeEntries $ Matrix (transpose $ getElems tr) ord len i
--}
{--
  case getElems elim of
    m | m == elems ->
      let tr = transpose elems in
      case eliminateEntries (Matrix tr order n) of
        Matrix es o  n | es == tr -> if b then Matrix (transpose tr) o n else Matrix tr o n
        m                         -> minimizeEntries True m
    m              ->
      case getIndex elim of
        i | i == length elems - 1 -> if b then Matrix (transpose m) order i
                                     else Matrix m order i
        i                         -> minimizeEntries False (Matrix m order i)
--}
{--}
findRow :: Integral a => Int -> [[a]] -> Maybe (Either Int (Int, Int))
findRow i [r]     =
  if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else Just $ Left i
findRow i (r:rs) =
  if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> findRow (i + 1) rs
      Just x  -> Just $ Right (i, x)
  else Just $ Left i

choosePivot :: Integral a => Matrix a -> (Maybe a, Matrix a)
choosePivot (Matrix elems ord len i) =
  case findRow i (drop i elems) of
    Nothing             -> (Nothing, Matrix elems ord len i)
    Just (Left x)       -> (Just $ elems !! x !! x, Matrix elems ord len i)
    Just (Right (x, y)) ->
      let pivot = elems !! x !! y
          newElems = map (switchElems x y) elems in
      (Just pivot, Matrix newElems ord len x)

colOperationHelper :: Integral a => Int -> [((a, a, a), Int)] -> [a] -> [a]
colOperationHelper _ [] row = row
colOperationHelper pIndex [((gcd, coeff1, coeff2), index)] row
  | index == pIndex = row
  | index < pIndex  =
    let first  = take index row
        second = drop (index + 1) (take pIndex row)
        third  = drop (pIndex + 1) row
        elem1  = row !! index
        elem2  = row !! pIndex
        q1     = elem1 `div` gcd
        q2     = elem2 `div` gcd in
    first ++ (((-q2)*elem1 + q1*elem2) : second) ++ ((coeff1*elem2 + coeff2*elem1) : third)
  | otherwise       =
    let first  = take pIndex row
        second = drop (pIndex + 1) (take index row)
        third  = drop (index + 1) row
        elem1  = row !! index
        elem2  = row !! pIndex
        q1     = elem1 `div` gcd
        q2     = elem2 `div` gcd in
    first ++ ((coeff1*elem2 + coeff2*elem1) : second) ++ (((-q2)*elem1 + q1*elem2) : third)
colOperationHelper pIndex (x:xs) row =
  colOperationHelper pIndex xs (colOperationHelper pIndex [x] row)

improvePivot :: Integral a => (a, Matrix a) -> Matrix a
improvePivot (pivot, Matrix elems ord len pIndex) =
  let row          = elems !! pIndex
      nonDivisList = filter (\n -> (fst n) `mod` pivot /= 0) (zip row [0..(length row - 1)])
      gcdList      = parMap (\n -> (extEucAlg pivot (fst n), snd n)) nonDivisList
      newElems     = parMap (colOperationHelper pIndex gcdList) elems in
  Matrix newElems ord len pIndex

eliminationHelper :: Integral a => a -> [a] -> [a] -> [a]
eliminationHelper _ [] []            = []
eliminationHelper elem (c:cs) (e:es) =
  (e - c*elem) : (eliminationHelper elem cs es)

eliminateEntries :: Integral a => Matrix a -> Matrix a
eliminateEntries (Matrix elems ord len pIndex) =
  let row      = elems !! pIndex
      pivot    = row !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> eliminationHelper (row !! pIndex) coeffs row) elems in
  Matrix newElems ord len pIndex

getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm matrix =
  case choosePivot matrix of
    (Nothing, _)  -> matrix
    (Just p, mat) ->
      let m      = improvePivot (p, mat)
          elim1  = eliminateEntries m
          pIndex = getIndex elim1
          ord    = getOrder elim1
          len    = getLength elim1
          elems  = getElems elim1
          pivot  = elems !! pIndex !! pIndex --zero??
          tr     = Matrix (transpose $ getElems elim1) ord len pIndex
          elim2  = eliminateEntries $ improvePivot (pivot, tr) in
      if pivot == 0 then getSmithNormalForm elim1 --maybe a bad fix
      else getSmithNormalForm elim2
--}