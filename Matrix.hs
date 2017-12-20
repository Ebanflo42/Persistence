--TODO:
--extensively document
module Matrix 
  ( IMatrix
  , BMatrix
  , multiply
  , multiplyPar
  , getSmithNormalFormInt
  , getSmithNormalFormIntPar
  , getSmithNormalFormBool
  , getUnsignedDiagonal
  , findKernelInt
  , findKernelBool
  ) where

import Util
import Data.List
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------
Matrices are transformed by iterating through each row and selecting a pivot
The pivot is the diagonal entry of the row, and must be non-zero
If the diagonal entry is non-zero at first, a switch is performed so that it is

To get the smith normal form, the entire pivot row and column is eliminated before continuing

To get the kernel, the matrix is first put into column eschelon form. To get column eschelon form,
every element in the pivot row after the pivot is eliminated. All column operations to get the
matrix into this form are also performed on the identiy matrix. Once this is over, the columns of
the former identity matrix corresponding to the zero columns of the column-eschelon form are the
basis of the kernel, the basis vectors are then made into the rows of a matrix

Eliminating elements is a slighltly more complicated process since integer operations are allowed only.
First, every element that must be eliminated is made divisible by the pivt using the bezout coefficients
from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used
to eliminate the elements.

Boolean matrices are regular matrices with elements modulo 2, Bool is an instance
of Num here and the instance is given in Util.
--}

--BASIC STUFF-------------------------------------------------------------

type IMatrix = [[Int]]
type BMatrix = [[Bool]]

--multiply matrices
multiply :: Num a => [[a]] -> [[a]] -> [[a]]
multiply mat1 mat2 = 
  let t = transpose mat2 in
  map (\row -> map (dotProduct row) t) mat1

--takes the transpose of a matrix in parallel
transposePar :: [[a]] -> [[a]]
transposePar mat =
  parMap rpar (\i -> map (\row -> row !! i) mat) [0..(length $ head mat) - 1]

--multiply matrices, evaluate rows in parallel if processors are available
multiplyPar :: Num a => [[a]] -> [[a]] -> [[a]]
multiplyPar mat1 mat2 = runEval $ do
  let t = transpose mat2
  rseq t
  return $ parMap rpar (\row -> map (dotProduct row) $ t) mat1

--gets the absolute value of every diagonal element, absolute value is identity for booleans in this case
getUnsignedDiagonal :: Num a => [[a]] -> [a]
getUnsignedDiagonal matrix =
  map (\i -> abs $ matrix !! i !! i) [0..(min (length matrix) (length $ head matrix)) - 1]

--SMITH NORMAL FORM OF INTEGER MATRICES-----------------------------------

--rearranges the matrix if necessary, returns the matrix paired with its pivot
--first argument is the index of the pivot row
choosePivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
choosePivotInt i mat = --assumes that i is a legal index for mat
  let pos = --Nothing if pivot is found, Right if a column switch needs to be performed, Left otherwise
        let row = mat !! i in
        if row !! i == 0 then
        case findIndex (\n -> n /= 0) row of
          Just x  -> Just $ Right (i, x)
          Nothing -> Nothing
        else if exactlyOneNonZero row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (Just $ mat !! x !! x, mat)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) mat in
      (Just $ newElems !! x !! x, newElems)
    Nothing           -> (Nothing, mat)

--does a column operation specific to Smith Normal Form algo on a single row
--good because parMap can be used to work on whole matrix in parallel
--first argument is the index of the pivot row, the second is the bezout coefficients,
--the quotients of the two elements by their gcd, and the index of the non-pivot element
colOperationHelperInt :: Int -> (Int, Int, Int, Int, Int) -> [Int] -> [Int]
colOperationHelperInt pIndex (c1, c2, q1, q2, index) row =
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

--does necessary rearranging if pivot is found
--returns pivot if it is found with the new matrix
chooseColumnPivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
chooseColumnPivotInt index elems =
  let pos =
        let col = map (\row -> row !! index) elems in
        if elems !! index !! index == 0 then
          case findIndex (\n -> n /= 0) col of
            Just x  -> Just $ Right (index, x)
            Nothing -> Nothing
        else if exactlyOneNonZero col then Nothing
        else Just $ Left index in
  case pos of
    Just (Left i)       -> (Just $ elems !! i !! i, elems)
    Just (Right (i, j)) ->
      let newElems = switchElems i j elems in
      (Just $ newElems !! i !! i, newElems)
    Nothing             -> (Nothing, elems)
    
--does the same thing as colOperationHelper except with rows, operates on the whole matrix
rowOperationHelper :: Int -> (Int, Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperationHelper pIndex (c1, c2, q1, q2, index) elems =
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

--same as rowOperationHelperPar except linear combinations of rows can be computed in parallel
rowOperationHelperPar :: Int -> (Int, Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperationHelperPar pIndex (c1, c2, q1, q2, index) elems =
  let row1 = elems !! index; row2 = elems !! pIndex in
  if index < pIndex then
    let first  = take index elems
        second = drop (index + 1) (take pIndex elems)
        third  = drop (pIndex + 1) elems
        res    = runEval $ do
          a <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          b <- rpar $ (c1 `mul` row2) `add` (c2 `mul` row1)
          rseq a
          rseq b
          return (a, b) in
    first ++ ((fst res):second)
    ++ ((snd res):third)
  else
    let first  = take pIndex elems
        second = drop (pIndex + 1) (take index elems)
        third  = drop (index + 1) elems
        res    = runEval $ do
          a <- rpar $ (c1 `mul` row1) `add` (c2 `mul` row2)
          b <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          rseq a
          rseq b
          return (a, b) in
    first ++ ((fst res):second)
    ++ ((snd res):third)

--given the pivot of a matrix and the matrix itself to improve that row with column operations
--makes every element in the row divisible by the pivot
--returns the new matrix paired with the new pivot
improveRowSmithInt :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveRowSmithInt pIndex (pivot, elems) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> (elems !! pIndex !! pIndex, elems)
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n --extended Euclidean algorithm, returns gcd tupled with coefficients that combine the args to get the gcd
          gcd          = one gcdTriple
          transform    = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
          newElems     = map (colOperationHelperInt pIndex transform) elems in
      improveRowSmithInt pIndex (newElems !! pIndex !! pIndex, newElems)

--same as above except row operations can be performed in parallel
improveRowSmithIntPar :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveRowSmithIntPar pIndex (pivot, elems) =
    let row          = elems !! pIndex
        nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
    case nonDivis of
      Nothing     -> (elems !! pIndex !! pIndex, elems)
      Just (n, i) ->
        let gcdTriple    = extEucAlg pivot n
            gcd          = one gcdTriple
            transform    = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
            newElems     = parMap rpar (colOperationHelperInt pIndex transform) elems in
        improveRowSmithInt pIndex (newElems !! pIndex !! pIndex, newElems)

--given pivot index and pivot paired with matrix whose pivot row has been improved, eliminates the entries in the pivot row
eliminateRowInt :: Int -> (Int, IMatrix) -> IMatrix
eliminateRowInt pIndex (pivot, elems) =
  let row      = elems !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  newElems

--same as above except it works on the pivot column
eliminateColInt :: Int -> (Int, IMatrix) -> IMatrix
eliminateColInt pIndex (pivot, elems) =
  let pRow     = elems !! pIndex
      coeffs   = map (\row -> let n = row !! pIndex in if n == 0 then 0 else n `div` pivot) elems in
  mapWithIndex (\i row -> if i == pIndex then row else row `subtr` (((coeffs) !! i) `mul` pRow)) elems

--given pivot index and pivot paired with matrix, improves pivot column with row operations
improveColInt :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveColInt pIndex (pivot, elems) =
  case indexAndElem (\row -> let n = row !! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
     Nothing     -> (elems !! pIndex !! pIndex, elems)
     Just (r, i) ->
       let n         = r !! pIndex
           gcdTriple = extEucAlg pivot n
           gcd       = one gcdTriple
           transform = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
           newElems  = rowOperationHelper pIndex transform elems in
       improveColInt pIndex (newElems !! pIndex !! pIndex, newElems)

--same as above except it uses rowOperationHelperPar
improveColIntPar :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveColIntPar pIndex (pivot, elems) =
    case indexAndElem (\row -> let n = row !! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
    Nothing     -> (elems !! pIndex !! pIndex, elems)
    Just (r, i) ->
        let n         = r !! pIndex
            gcdTriple = extEucAlg pivot n
            gcd       = one gcdTriple
            transform = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
            newElems  = rowOperationHelperPar pIndex transform elems in
        improveColIntPar pIndex (newElems !! pIndex !! pIndex, newElems)

--gets the Smith normal form of an integer matrix
getSmithNormalFormInt :: IMatrix -> IMatrix
getSmithNormalFormInt matrix =
  let maxIndex     = min (length matrix) (length $ head matrix)
      calc i m mat =
        if i == m then mat else
          case choosePivotInt i mat of
            (Nothing, _)  ->
              case chooseColumnPivotInt i mat of
                (Nothing, _)  -> calc (i + 1) m mat
                (Just p, new) -> calc (i + 1) m $ (eliminateColInt i) $ improveColInt i (p, new)
            (Just p, new) ->
              case chooseColumnPivotInt i $ (eliminateRowInt i) $ improveRowSmithInt i (p, new) of
                (Nothing, new') -> calc (i + 1) m new'
                (Just p, new')  -> calc (i + 1) m $ (eliminateColInt i) $ improveColInt i (p, new') in
  calc 0 maxIndex matrix

--gets the Smith normal form of a matrix, uses lots of parallelism if possible
getSmithNormalFormIntPar :: IMatrix -> IMatrix
getSmithNormalFormIntPar matrix =
  let maxIndex     = min (length matrix) (length $ head matrix)
      calc i m mat =
        if i == m then mat else
          case choosePivotInt i mat of
            (Nothing, _)  ->
              case chooseColumnPivotInt i mat of
                (Nothing, _)  -> calc (i + 1) m mat
                (Just p, new) -> calc (i + 1) m $ (eliminateColInt i) $ improveColIntPar i (p, new)
            (Just p, new)   ->
              case chooseColumnPivotInt i $ (eliminateRowInt i) $ improveRowSmithIntPar i (p, new) of
                (Nothing, new')   -> calc (i + 1) m new'
                (Just p, new') -> calc (i + 1) m $ (eliminateColInt i) $ improveColIntPar i (p, new') in
    calc 0 maxIndex matrix

--SMITH NORMAL FORM OF BOOLEAN MATRICES-----------------------------------

--rearranges the matrix if necessary, returns the matrix paired with its pivot
--first argument is the index of the pivot row
choosePivotBool :: Int -> BMatrix -> (Bool, BMatrix)
choosePivotBool i mat =
  let pos =
        let row = mat !! i in
        if not $ row !! i then
          case findIndex id row of
            Just x  -> Just $ Right (i, x)
            Nothing -> Nothing
        else if exactlyOneTrue row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat)
    Just (Right (x, y)) -> (True, map (switchElems x y) mat)
    Nothing             -> (False, mat)

--switches rows if necessary to get the value at the pivot position to be 1
chooseColumnPivotBool :: Int -> BMatrix -> (Bool, BMatrix)
chooseColumnPivotBool index elems =
  if elems !! index !! index then (True, elems) else
    case indexAndElem (\row -> row !! index) elems of
      Nothing     -> (False, elems)
      Just (_, i) -> (True, switchElems i index elems)

--eliminates pivot row of a boolean matrix
elimRowSmithBool :: Int -> BMatrix -> BMatrix
elimRowSmithBool pIndex mat =
  let row          = mat !! pIndex
      indices      = elemIndices True row
      calc is m    =
        case is of
          (x:xs) -> 
            if x == pIndex then calc xs m
            else calc xs $ applyColumnOp (SimpleAdd (pIndex, x)) mat
          []     -> m in
  calc indices mat

--eliminates pivot column of a boolean matrix
elimColBool :: Int -> BMatrix -> BMatrix
elimColBool pIndex elems =
  let pRow     = elems !! pIndex in
      mapWithIndex (\i row -> if (not $ row !! pIndex) || i == pIndex then row else row `add` pRow) elems

--gets the Smith normal form of a boolean (mod 2) matrix, no parallel version because its very cheap
getSmithNormalFormBool :: BMatrix -> BMatrix
getSmithNormalFormBool matrix =
    let maxIndex     = min (length matrix) (length $ head matrix)
        calc i m mat =
            if i == m then mat else
            case choosePivotBool i mat of
                (False, new)  ->
                  case chooseColumnPivotBool i new of
                    (False, _)   -> calc (i + 1) m new
                    (True, new') -> calc (i + 1) m $ elimColBool i new'
                (True, new)   ->
                  case chooseColumnPivotBool i $ elimRowSmithBool i new of
                    (False, new') -> calc (i + 1) m new'
                    (True, new')  -> calc (i + 1) m $ elimColBool i new' in
    calc 0 maxIndex matrix

--KERNEL OF INTEGER MATRICES----------------------------------------------

--this data stucture was originally used to record all column operations used to get the matrix into column eschelon form
--it is still somewhat in use, but I might eliminate it
data ColumnOp a =
    Insert Int Int --insert column at first index at second index
  | Combine Int Int (a, a) (a, a) --transform two columns into linear combinations of each other, coefficients for respective columns are in the tuples
  | Add (Int, Int) a --add column at first index to the one at the second index after multiplying by the coeff
  | SimpleAdd (Int, Int) --add first column to the second
  | Switch Int Int --switch columns, first index is lower than second
    deriving Show

--apply a column operation to a matrix
applyColumnOp :: Num a => ColumnOp a -> [[a]] -> [[a]]
applyColumnOp op matrix =
  case op of
    Insert i j              -> --assume j > i
      map (\row -> (take i row) ++ (drop (i + 1) $ take (j + 1) row) ++ ((row !! i):(drop (j + 1) row))) matrix
    Switch i j              ->
      map (\row -> (take i row) ++ ((row !! j):(drop (i + 1) $ take j row)) ++ ((row !! i):(drop (j + 1) row))) matrix
    SimpleAdd (i, j)        ->
      map (\row -> (take j row) ++ (((row !! i) + (row !! j)):(drop (j + 1) row))) matrix
    Combine i j (a, b) (c, d) -> --assume j > i
      map (\row ->
        (take i row) ++ (a*(row !! i) + b*(row !! j):(drop (i + 1) $ take j row)) ++ ((c*(row !! i) + d*(row !! j)):(drop (j + 1) row))) matrix
    Add (i, j) coeff        ->
      map (\row -> (take j row) ++ ((coeff*(row !! i) + (row !! j)):(drop (j + 1) row))) matrix

--applys a sequence of column operations by repeatedly calling applyColumnOp
applyColumnOps :: Num a => [ColumnOp a] -> [[a]] -> [[a]]
applyColumnOps operations matrix =
  case operations of
    (op:ops) -> applyColumnOps ops $ applyColumnOp op matrix
    []       -> matrix

--preps the matrix for gauss-jordan elimination
--returns the index of the last non-zero row, the prepped matrix,
--and the identity matrix with the same column operations performed
moveAllZeroColsBackInt :: IMatrix -> IMatrix -> (Int, IMatrix, IMatrix)
moveAllZeroColsBackInt elems identity =
  let len    = (length $ head elems) - 1
      zeroes = filter (\i -> forall (\row -> row !! i == 0) elems) [0..len]
      ops    = map (\i -> Insert i len) zeroes in
  (len - (length zeroes), applyColumnOps ops elems, applyColumnOps ops identity)

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
chooseGaussPivotInt :: Int -> IMatrix -> (Bool, IMatrix, Maybe (ColumnOp Int))
chooseGaussPivotInt i mat = --assumes that i is a legal index for mat
  let row  = mat !! i
      elem = row !! i
      pos  =
        if elem == 0 then
        case filter (\index -> index > i) $ findIndices (\n -> n /= 0) row of
          (x:_)  -> Just $ Right (i, x)
          []     -> Nothing
        else if exactlyOneNonZero row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat, Nothing)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) mat in
      (True, newElems, Just $ Switch x y)
    Nothing             -> (False, mat, Nothing)

--makes every element after the pivot in the pivot row divisible by the pivot
--returns improved matrix (first), new pivot, and the identity with the same column operations performed (third)
improveRowGaussInt :: Int -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowGaussInt pIndex maxIndex elems identity =
  let improve i mat ide =
        if i == maxIndex then (mat, mat !! pIndex !! pIndex, ide) else
          let row   = mat !! pIndex
              pivot = row !! pIndex
              x     = row !! i
              next  = i + 1 in
          if x == 0 || (x `mod` pivot == 0) then
            improve next mat ide else
            let gcdTriple = extEucAlg pivot x
                gcd       = one gcdTriple
                transform = map (colOperationHelperInt pIndex (two gcdTriple, thr gcdTriple, pivot `div` gcd, x `div` gcd, i)) in
            improve next (transform mat) (transform ide) in
  improve (pIndex + 1) elems identity

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
eliminateEntriesGaussInt :: Int -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
eliminateEntriesGaussInt pIndex maxIndex (elems, pivot, identity) =
  let row            = elems !! pIndex
      elim i mat ide =
        if i == maxIndex then (mat, ide)
        else 
          let coeff     = (row !! i) `div` pivot
              transform = map (\r -> (take i r) ++ (((r !! i) - coeff*(r !! pIndex)):(drop (i + 1) r))) in
          elim (i + 1) (transform mat) (transform ide) in
  elim (pIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernelInt :: IMatrix -> IMatrix
findKernelInt matrix =
  let len      = length matrix
      len0     = length $ head matrix
      len01    = len0 - 1
      maxIndex = if len > len0 then len0 else len
      identity = map (\i -> (replicate i 0) ++ (1:(replicate (len01 - i) 0))) [0..len01]
      zeroData = moveAllZeroColsBackInt matrix identity
      doColOps index (elems, ide) =
        if index == (one zeroData) || index == maxIndex then (elems, ide) else 
          case chooseGaussPivotInt index elems of
            (True, _, Nothing)  ->
              doColOps (index + 1) $ eliminateEntriesGaussInt index len0 $ improveRowGaussInt index len0 elems ide
            (True, mx, Just op) ->
              doColOps (index + 1) $ eliminateEntriesGaussInt index len0 $ improveRowGaussInt index len0 mx $ applyColumnOp op ide
            (False, _, _)       -> doColOps (index + 1) (elems, ide)
      result   = doColOps 0 (not1 zeroData)
      elems    = fst result
      ide      = snd result in
  map (\i -> map (\row -> row !! i) ide) $ filter (\i -> forall (\row -> row !! i == 0) elems) [0..len01]

--KERNEL OF BOOLEAN MATRICES----------------------------------------------

--preps the matrix for gauss-jordan elimination
--returns the index of the last non-zero row, the prepped matrix,
--and the identity matrix with the same column operations performed
moveAllZeroColsBackBool :: BMatrix -> BMatrix -> (Int, BMatrix, BMatrix)
moveAllZeroColsBackBool elems identity =
  let len    = (length $ head elems) - 1
      zeroes = filter (\i -> forall (\row -> row !! i == 0) elems) [0..len]
      ops    = map (\i -> Insert i len) zeroes in
  (len - (length zeroes), applyColumnOps ops elems, applyColumnOps ops identity)

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
chooseGaussPivotBool :: Int -> BMatrix -> (Bool, BMatrix, Maybe (ColumnOp Bool))
chooseGaussPivotBool i mat = --assumes that i is a legal index for mat
  let row  = mat !! i
      elem = row !! i
      pos  =
        if not elem then
        case filter (\index -> index > i) $ findIndices (\n -> n /= 0) row of
          (x:_)  -> Just $ Right (i, x)
          []     -> Nothing
        else if exactlyOneTrue row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat, Nothing)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) mat in
      (True, newElems, Just $ Switch x y)
    Nothing             -> (False, mat, Nothing)

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
eliminateEntriesGaussBool :: Int -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
eliminateEntriesGaussBool pIndex maxIndex elems identity =
  let row            = elems !! pIndex
      len            = length $ head elems
      elim i mat ide =
          if i == len then (mat, ide)
          else let transform = map (\row -> (take i row) ++ (((row !! i) + (row !! pIndex)):(drop (i + 1) row))) in
            elim (i + 1) (transform mat) (transform ide) in
  elim (pIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernelBool :: BMatrix -> BMatrix
findKernelBool matrix =
  let len      = length matrix
      len0     = length $ head matrix
      len01    = len0 - 1
      maxIndex = if len > len0 then len0 else len
      identity = map (\i -> (replicate i False) ++ (True:(replicate (len01 - i) False))) [0..len01]
      zeroData = moveAllZeroColsBackBool matrix identity
      doColOps index (elems, ide) =
        if index == (one zeroData) || index == maxIndex then (elems, ide) else 
          case chooseGaussPivotBool index elems of
            (True, _, Nothing)  ->
              doColOps (index + 1) $ eliminateEntriesGaussBool index len0 elems ide
            (True, mx, Just op) ->
              doColOps (index + 1) $ eliminateEntriesGaussBool index len0 mx $ applyColumnOp op ide
            (False, _, _)       -> doColOps (index + 1) (elems, ide)
      result   = doColOps 0 (not1 zeroData)
      elems    = fst result
      ide      = snd result in
  map (\i -> map (\row -> row !! i) ide) $ filter (\i -> forall (\row -> not $ row !! i) elems) [0..len01]