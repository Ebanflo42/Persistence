module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] Bool Int Int deriving Show

getElems (Matrix elems _ _ _) = elems --elements of the matrix
isMod2 (Matrix _ modulo2 _ _) = modulo2 --modulus of the elements
getIndex (Matrix _ _ index _) = index --index of the current pivot
getMaxpIndex (Matrix _ _ _ m) = m     --maximum pivot index (minimum of dimensions)

--given the *order* of the group and the elements of the matrix
--calculates the maximum pivot index and sets initial pivot index to zero
initializeMatrix :: Integral a => Bool -> [[a]] -> Matrix a
initializeMatrix ismod2 elems =
  Matrix elems ismod2 0 ((min (length $ elems !! 0) (length elems)) - 1)

--increments the pivot index
incrementIndex :: Integral a => Matrix a -> Matrix a
incrementIndex (Matrix a b i c) = Matrix a b (i + 1) c

--shows the rows of an integer matrix separated by new lines
--as well as the modulus of the matrix
toString :: Matrix Int -> String
toString matrix =
  let mat = flatten $ map (\row -> '\n':(flatten $ map (\e -> ' ':(show e)) row)) (getElems matrix) in
  mat ++ (if isMod2 matrix then "\nmodulo 2" else "")

--multiply two matrices
multiply :: Integral a => Matrix a -> Matrix a -> Matrix a
multiply (Matrix e1 o1 _ _) (Matrix e2 o2 _ _) =
  if o1 /= o2 then error "Matrices were not of the same modulus."
  else let right = transpose e2; newElems = map (\row -> map (dotProduct row) right) e1 in
    initializeMatrix o1 newElems

--first argument is a looping index, second argument is the upper bound for that index
--if a pivot row isn't found return nothing
--if it is found and the diagonal element is non-zero return the pivot index
--if it is found and is zero return the row index and the column index of the first non-zero element in the row
pivotHelper :: Integral a => Int -> Int -> [[a]] -> Maybe (Either Int (Int, Int))
pivotHelper _ _ []      = error "Empty matrix passed to pivotHelper."
pivotHelper i max (r:_) =
  if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else if exactlyOneNonZero r then Nothing
  else Just $ Left i

--calls pivot helper, rearranges the matrix if necessary, returns the matrix pair with its pivot
choosePivot :: Integral a => Matrix a -> (Maybe a, Matrix a)
choosePivot (Matrix elems ord i max) =
  case pivotHelper i max (drop i elems) of
    Nothing             -> (Nothing, Matrix elems ord i max)
    Just (Left x)       -> (Just $ elems !! x !! x, Matrix elems ord x max)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) elems in
      (Just $ newElems !! x !! x, Matrix newElems ord x max)

--given the index of the pivot, a pair, and a row of a matrix
--the first part of the pair is the gcd, bezout coefficients, and quotients
--second part of the pair is the index of the element to which that applies
colOperationHelper :: Integral a => Int -> ((a, a, a, a, a), Int) -> [a] -> [a]
colOperationHelper pIndex ((gcd, c1, c2, q1, q2), index) row =
  let elem1 = row !! index; elem2 = row !! pIndex in
  if index < pIndex then
    let first  = take index row
        second = drop (index + 1) (take pIndex row)
        third  = drop (pIndex + 1) row
        elem1  = row !! index
        elem2  = row !! pIndex in
    first ++ ((q2*elem1 - q1*elem2) : second) ++ ((c1*elem2 + c2*elem1) : third)
  else
    let first  = take pIndex row
        second = drop (pIndex + 1) (take index row)
        third  = drop (index + 1) row in
    first ++ ((c1*elem1 + c2*elem2) : second) ++ ((q2*elem1 - q1*elem2) : third)

--given the pivot of a matrix and the matrix itself to improve that row with column operations
improvePivot :: Integral a => (a, Matrix a) -> Matrix a
improvePivot (pivot, Matrix elems ord pIndex max) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> Matrix elems ord pIndex max
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n
          gcd          = one gcdTriple
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems     = map (colOperationHelper pIndex transform) elems in
      improvePivot (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

improvePivotPar :: Integral a => (a, Matrix a) -> Matrix a
improvePivotPar (pivot, Matrix elems ord pIndex max) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> Matrix elems ord pIndex max
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n
          gcd          = one gcdTriple
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems     = parMap (colOperationHelper pIndex transform) elems in
      improvePivot (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

--given a matrix whose pivot row has been improved, eliminates the entries in that row
eliminateEntries :: Integral a => a -> Matrix a -> Matrix a
eliminateEntries pivot (Matrix elems ord pIndex max) =
  let row      = elems !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  Matrix newElems ord pIndex max

eliminateEntriesPar :: Integral a => a -> Matrix a -> Matrix a
eliminateEntriesPar pivot (Matrix elems ord pIndex max) =
  let row      = elems !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> parMapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  Matrix newElems ord pIndex max

--attempt at eliminating the transpose at every step
{--
columnOperations :: Integral a => Matrix a -> Matrix a
columnOperations (Matrix elems ord index max) =
  if index == max then Matrix elems ord 0 max else
  case choosePivot (Matrix elems ord index max) of
    (Nothing, _)  -> columnOperations $ Matrix elems ord (index + 1) max
    (Just p, mat) -> (columnOperations . eliminateEntries . improvePivot) (p, mat)

getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm matrix =
  columnOperations $ Matrix ((transpose . getElems . columnOperations) matrix) (getOrder matrix) 0 (getMaxIndex matrix)
--}
{--}
--gets the smith normal form of a matrix
getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm (Matrix elems ord index max) =
  if index > max then Matrix elems ord index max else
  case choosePivot $ Matrix elems ord index max of
    (Nothing, _)  ->
      let tr = Matrix (transpose elems) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalForm . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalForm . incrementIndex . (eliminateEntries p) . improvePivot) (p, mat)
    (Just p, mat) ->
      let tr = Matrix ((transpose . getElems . (eliminateEntries p) . improvePivot) (p, mat)) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalForm . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalForm . incrementIndex . (eliminateEntries p) . improvePivot) (p, mat)
--}
{--
getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm (Matrix elems ord 0 max) =
  let eliminateRows (Matrix e o i m) =
        if i > m then Matrix e o i m else
        case choosePivot $ Matrix e o i m of
          (Nothing, _)  -> eliminateRows $ Matrix e o (i + 1) m
          (Just p, mat) -> (eliminateRows . incrementIndex . eliminateEntries . improvePivot) (p, mat)
      firstRound                     = eliminateRows $ Matrix elems ord 0 max in
  eliminateRows $ Matrix (getElems firstRound) ord 0 max
--}
getSmithNormalFormPar :: Integral a => Matrix a -> Matrix a
getSmithNormalFormPar (Matrix elems ord index max) =
  if index == max then Matrix elems ord index max else
  case choosePivot $ Matrix elems ord index max of
    (Nothing, _)  ->
      let tr = Matrix (transpose elems) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalFormPar . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalFormPar . incrementIndex . (eliminateEntriesPar p) . improvePivotPar) (p, mat)
    (Just p, mat) ->
      let tr = Matrix ((transpose . getElems . (eliminateEntriesPar p) . improvePivotPar) (p, mat)) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalFormPar . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalFormPar . incrementIndex . (eliminateEntriesPar p) . improvePivotPar) (p, mat)

getUnsignedDiagonal :: Integral a => Matrix a -> [a]
getUnsignedDiagonal matrix =
  let f = if isMod2 matrix then \n -> mod n 2 else abs
      helper _ []     = []
      helper i (x:xs) =
        if i > getMaxpIndex matrix then []
        else (f $ x !! i) : (helper (i + 1) xs) in
  helper 0 (getElems matrix)

--switch two columns or
--add two columns specified by indicies of fst field with coefficients from snd, destination row given by second index 
data ColumnOp a = Switch (Int, Int) | Combo ((Int, Int), (a, a))

--preps the matrix for gauss-jordan and returns the index of the last non-zero row
moveAllZeroRowsBack :: Integral a => Matrix a -> (Int, Matrix a)
moveAllZeroRowsBack (Matrix elems o i m) =
  let zeroes = myfilter (\row -> forall (\n -> n == 0) row) elems in
    ((length elems) - (length $ two zeroes), Matrix ((thr zeroes) ++ (one zeroes)) o i m)

{--
findGaussianPivot :: Integral a => Matrix a -> Maybe (Int, Int, a, Matrix a)
findGaussianPivot (Matrix elems ismod2 index max) =
  let helper _ []     = Nothing
      helper i (x:xs) =
        if i > max then Nothing else
        case indexAndElem (\a -> a /= 0) x of
          Nothing     -> helper (i + 1) xs
          Just (e, j) -> Just (i, j, e, Matrix elems ismod2 i max) in
  helper index elems
--}

improvePivotGauss :: Integral a => (a, Matrix a) -> Matrix a
improvePivotGauss (pivot, Matrix elems ismod2 pIndex max) =
  let improve ((n, i):xs) mat =
        let gcdTriple    = extEucAlg pivot n
            gcd          = one gcdTriple
            transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
            newElems     = map (colOperationHelper pIndex transform) $ getElems mat in
        improve xs $ Matrix newElems ismod2 pIndex max
      improve [] mat          = mat in
  improve (filter (\pair -> snd pair > pIndex) $ indexAndElems (\n -> n /= 0 && n `mod` pivot /=0) (elems !! pIndex)) $ Matrix elems ismod2 pIndex max

eliminateEntriesGauss :: Integral a => a -> Matrix a -> Matrix a
eliminateEntriesGauss pivot (Matrix elems ismod2 pIndex max) =
  let row          = elems !! pIndex
      len          = length elems
      helper i mat =
        if i == len then mat
        else let coeff = (row !! i) `div` pivot in
             helper (i + 1) $
               Matrix (map (\r -> (take i r) ++ (((r !! i) - coeff*(r !! pIndex)):(drop (i + 1) r))) (getElems mat)) ismod2 pIndex max in
  helper (pIndex + 1) $ Matrix elems ismod2 pIndex max

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernel :: Integral a => Matrix a -> Matrix a
findKernel matrix =
  let doRowOps (Matrix elems ord index max) =
        if index > max then Matrix elems ord index max
        else case choosePivot $ Matrix elems ord index max of
               (Nothing, m)  -> (findKernel . incrementIndex) m
               (Just p, mat) -> (findKernel . incrementIndex . (eliminateEntriesGauss p) . improvePivot) (p, mat) in
  (doRowOps . snd . moveAllZeroRowsBack) matrix