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

--insert column at one index at a different index or
--add two columns specified by indicies of fst field with coefficients from snd, destination row given by second index 
data ColumnOp a = Insert Int Int | Combo (Int Int (a, a) (a, a) | Add (Int, Int) a

applyColumnOps :: Integral a => [ColumnOp a] -> [[a]] -> [[a]]
applyColumnOps operations matrix =
  case operations of
    (op:ops) ->
      case op of
        Insert i j                 ->
          applyColumnOps ops
            map (\row -> (take i row) ++ (drop (i + 1) row) ++ (drop i $ take j row) ++ ((row !! i):(drop j row))) matrix
        Combo (i, j) (a, b) (c, d) ->
          applyColumnOps ops
            map (\row ->
              (take i row) ++ (a*(row !! i) + b*(row !! j):(drop i $ take j row)) ++ ((c*(row !! i) + d*(row !! j)):(drop (j + 1) row)) matrix
        Add (i, j) coeff            ->
          map (\row -> (take j row) ++ ((coeff*(row !! i) + (row !! j)):(drop (j + 1) row))) matrix
    []       -> matrix

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
        third  = drop (pIndex + 1) row in
    first ++ ((q2*elem1 - q1*elem2) : second) ++ ((c1*elem2 + c2*elem1) : third)
  else
    let first  = take pIndex row
        second = drop (pIndex + 1) (take index row)
        third  = drop (index + 1) row in
    first ++ ((c1*elem1 + c2*elem2) : second) ++ ((q2*elem1 - q1*elem2) : third)

rowOperationHelper :: Integral a => Int -> ((a, a, a, a, a), Int) -> [[a]] -> [[a]]
rowOperationHelper pIndex ((gcd, c1, c2, q1, q2), index) elems =
  let row1 = elems !! index; row2 = elems !! pIndex in
  if index < pIndex then
    let first  = take index elems
        second = drop (index + 1) (take pIndex elems)
        third  = drop (pIndex + 1) elems in
    first ++ (((q2 `mul` row1) `subtr` (q1 `mul` row2)) : second)
      ++ (((c1 `mul` row2) `add` (c2 `mul` row1)) : third)
  else
    let first  = take pIndex elems
        second = drop (pIndex + 1) (take index elems)
        third  = drop (index + 1) elems in
    first ++ (((c1 `mul` row1) `add` (c2 `mul` row2)) : second)
      ++ (((q2 `mul` row1) `subtr` (q1 `mul` row2)) : third)

--given the pivot of a matrix and the matrix itself to improve that row with column operations
--makes every element in the row divisible by the pivot
improveRowSmith :: Integral a => (a, Matrix a) -> Matrix a
improveRowSmith (pivot, Matrix elems ord pIndex max) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> Matrix elems ord pIndex max
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n
          gcd          = one gcdTriple
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems     = map (colOperationHelper pIndex transform) elems in
      improveRowSmith (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

improveRowSmithPar :: Integral a => (a, Matrix a) -> Matrix a
improveRowSmithPar (pivot, Matrix elems ord pIndex max) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> Matrix elems ord pIndex max
    Just (n, i) ->
      let gcdTriple = extEucAlg pivot n
          gcd       = one gcdTriple
          transform = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems  = parMap (colOperationHelper pIndex transform) elems in
      improveRowSmith (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

--given pivot and matrix, improves pivot column with row operations
improveCol :: Integral a => (a, Matrix a) -> Matrix a
improveCol (pivot, Matrix elems ord pIndex max) =
  case indexAndElem (\row -> let n = row !! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
     Nothing     -> Matrix elems ord pIndex max
     Just (r, i) ->
      let n         = r !! pIndex
          gcdTriple = extEucAlg pivot n
          gcd       = one gcdTriple
          transform = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems  = rowOperationHelper pIndex transform elems in
      improveCol $ (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

--given a matrix whose pivot row has been improved, eliminates the entries in that row
eliminateRow :: Integral a => a -> Matrix a -> Matrix a
eliminateRow pivot (Matrix elems ord pIndex max) =
  let row      = elems !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  Matrix newElems ord pIndex max

eliminateCol :: Integral a => a -> Matrix a -> Matrix a
eliminateCol pivot (Matrix elems ord pIndex max) =
  let pRow     = elems !! pIndex
      coeffs   = map (\row -> let n = row !! pIndex in if n == 0 then 0 else n `div` pivot) elems
      newElems = mapWithIndex (\i row -> if i == pIndex then row else row `subtr` (((coeffs) !! i) `mul` pRow)) elems in
  Matrix newElems ord pIndex max

--gets the smith normal form of a matrix
getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm (Matrix elems ord index max) =
  if index > max then Matrix elems ord index max else
  case choosePivot $ Matrix elems ord index max of
    (Nothing, _)  ->
      let tr = Matrix (transpose elems) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalForm . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalForm . incrementIndex . (eliminateRow p) . improveRowSmith) (p, mat)
    (Just p, mat) ->
      let tr = Matrix ((transpose . getElems . (eliminateRow p) . improveRowSmith) (p, mat)) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalForm . incrementIndex) tr
        (Just p, mat) -> (getSmithNormalForm . incrementIndex . (eliminateRow p) . improveRowSmith) (p, mat)
--}
getSmithNormalFormPar :: Integral a => Matrix a -> Matrix a
getSmithNormalFormPar (Matrix elems ord index max) =
  if index == max then Matrix elems ord index max else
  case choosePivot $ Matrix elems ord index max of
    (Nothing, _)  ->
      let tr = Matrix (transpose elems) ord index max in
      case choosePivot tr of
        (Nothing, _) -> (getSmithNormalFormPar . incrementIndex) tr
        (Just p, mx) -> (getSmithNormalFormPar . incrementIndex . (eliminateRow p) . improveRowSmithPar) (p, mx)
    (Just p, mat) ->
      let tr = Matrix ((transpose . getElems . (eliminateRow p) . improveRowSmithPar) (p, mat)) ord index max in
      case choosePivot tr of
        (Nothing, _)  -> (getSmithNormalFormPar . incrementIndex) tr
        (Just p, mx) -> (getSmithNormalFormPar . incrementIndex . (eliminateRow p) . improveRowSmithPar) (p, mx)

columnPivotHelper :: Integral a => Int -> Int -> [[a]] -> Maybe (Either Int (Int, Int))
columnPivotHelper _ _ []      = error "Empty matrix passed to columnPivotHelper."
columnPivotHelper i max elems =
  let col = map (\row -> row !! i) elems in
  if elems !! i !! i == 0 then
    case findIndex (\n -> n /= 0) col of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else if exactlyOneNonZero col then Nothing
  else Just $ Left i

chooseColumnPivot :: Integral a => Matrix a -> (Maybe a, Matrix a)
chooseColumnPivot (Matrix elems ismod2 index max) =
  case columnPivotHelper index max elems of
    Nothing             -> (Nothing, Matrix elems ismod2 index max)
    Just (Left i)       -> (Just $ elems !! i !! i, Matrix elems ismod2 index max)
    Just (Right (i, j)) ->
      let newElems = switchElems i j elems in
      (Just $ newElems !! i !! i, Matrix newElems ismod2 index max)

getSmithNormalForm1 :: Integral a => Matrix a -> Matrix a
getSmithNormalForm1 (Matrix elems ord index max) =
  if index > max then Matrix elems ord index max else
  case choosePivot $ Matrix elems ord index max of
    (Nothing, mat)  ->
      case chooseColumnPivot mat of
        (Nothing, _)  -> (getSmithNormalForm1 . incrementIndex) mat
        (Just p, mat) -> (getSmithNormalForm1 . incrementIndex . (eliminateCol p) . improveCol) (p, mat)
    (Just p, mat)   ->
      case (chooseColumnPivot . (eliminateRow p) . improveRowSmith) (p, mat) of
        (Nothing, mat) -> (getSmithNormalForm1 . incrementIndex) mat
        (Just p, mat)  -> (getSmithNormalForm1 . incrementIndex . (eliminateCol p) . improveCol) (p, mat)

getUnsignedDiagonal :: Integral a => Matrix a -> [a]
getUnsignedDiagonal matrix =
  let f = if isMod2 matrix then \n -> mod n 2 else abs
      helper _ []     = []
      helper i (x:xs) =
        if i > getMaxpIndex matrix then []
        else (f $ x !! i) : (helper (i + 1) xs) in
  helper 0 (getElems matrix)

--preps the matrix for gauss-jordan and returns the index of the last non-zero row
moveAllZeroRowsBack :: Integral a => Matrix a -> (Int, Matrix a, [ColumnOp a])
moveAllZeroRowsBack (Matrix elems o i m) =
  let zeroes = myfilter (\row -> forall (\n -> n == 0) row) elems
      len    = length $ head elems in
    (len - (length $ two zeroes), Matrix ((thr zeroes) ++ (one zeroes)) o i m, map (\i -> Insert i (len - 1)) $ two zeroes)

{--
choosePivotGauss :: Integral a => Matrix a -> (Maybe a, Matrix a)
choosePivotGauss (Matrix elems ismod2 index max) =
  let row = elems !! index; elem = row !! index in
  if elem == 0 then
    if index == 0 then
      case indexAndElem (\n -> n /= 0) row of
        Nothing      -> (Nothing, Matrix elems ismod2 index max)
        Just (x, i)  -> (Just x, Matrix (map (switchElems i index) elems) ismod2 index max)
    else if index == max then
      let newElems = map (\row -> (take (max - 1) row) ++ [row !! max, row !! (max - 1)]) elems in
        (Just $ newElems !! index !! index, Matrix newElems ismod2 index max)
    else let newElems = map (switchConsecutive index) elems in
      (Just $ newElems !! index !! index, Matrix newElems ismod2 index max)
  else (Just elem, Matrix elems ismod2 index max)
--}
improveRowGauss :: Integral a => (a, Matrix a) -> ([ColumnOp a], Matrix a)
improveRowGauss (pivot, Matrix elems ismod2 pIndex max) =
  let improve ops list mat =
        case list of
          ((n, i):xs) ->
            let gcdTriple    = extEucAlg pivot n
                gcd          = one gcdTriple
                transform    = ((gcd, two gcdTriple, thr gcdTriple, pivot `div` gcd, n `div` gcd), i)
                newElems     = map (colOperationHelper pIndex transform) $ getElems mat in
            improve ((Combo (pIndex, i) (two gcdTriple, thr gcdTriple) (five4 trasnform) (five5 transform):ops) xs $
              Matrix newElems ismod2 pIndex max
          []          -> (ops, mat) in
  improve [] (filter (\pair -> snd pair > pIndex) $ indexAndElems (\n -> n /= 0 && n `mod` pivot /= 0) (elems !! pIndex)) $
         Matrix elems ismod2 pIndex max

eliminateEntriesGauss :: Integral a => a -> Matrix a -> ([ColumnOp a], Matrix a)
eliminateEntriesGauss pivot (Matrix elems ismod2 pIndex max) =
  let row            = elems !! pIndex
      len            = length $ elems !! 0
      elim ops i mat =
        if i == len then (ops, mat)
        else let coeff = (row !! i) `div` pivot in
             elim ((Add ((pIndex, i), coeff)):ops) (i + 1) $
               Matrix (map (\r -> (take i r) ++ (((r !! i) - coeff*(r !! pIndex)):(drop (i + 1) r))) (getElems mat)) ismod2 pIndex max in
  elim (pIndex + 1) $ Matrix elems ismod2 pIndex max

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernel :: Integral a => Matrix a -> Matrix a
findKernel matrix =
  let doColOps opList (Matrix elems ord index max) i =
        if index == i || index == max then (opList, Matrix elems ord index max)
        else case choosePivot $ Matrix elems ord index max of
               (Nothing, m) -> doColOps opList $ incrementIndex m
               (Just p, mx) ->
                let improved = improveRowGauss (p, mx)
                    elim     = eliminateEntriesGauss p $ snd improved in
                doColOps (opList ++ (fst improved) ++ (fst elim))
                  (incrementIndex $ snd elim)
      zeroData    = moveAllZeroRowsBack matrix
      gaussElim   = doColOps (thr zeroData) matrix (one zeroData)
      identity    = doColOps (fst gaussElim) $ replicate (length elems) (replicate (length $ head elems) 0)
      elems       = getElems $ snd gaussElim in
  map (\i -> map (\row -> row !! i) identity) $ filter (\i -> forall (\row -> row !! i == 0) elems) [0..length elems - 1]