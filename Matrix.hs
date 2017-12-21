--TODO:
--extensively document
module Matrix 
  ( IMatrix
  , BMatrix
  , transposeMat
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
import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------
Matrices are transformed by iterating through each row and selecting a pivot
The pivot is the diagonal entry of the row, and must be non-zero
If the diagonal entry is non-zero at first, a switch is performed so that it is

To get the smith normal form, the entire pivot row and column is eliminated before continuing

To get the kernel, the matrix is first put into column eschelon form. To get column eschelon form,
every element in the pivot row after the pivot is eliminated. All column operations to get the
IMatrixo this form are also performed on the identiy matrix. Once this is over, the columns of
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

type IMatrix = Vector (Vector Int)
type BMatrix = Vector (Vector Bool)

transposeMat :: Vector (Vector a) -> Vector (Vector a)
transposeMat mat =
  vectorFromList $ L.map (\i -> V.map (\row -> row ! i) mat) [0..(V.length $ V.head mat) - 1]

--takes the transpose of a matrix in parallel
transposePar :: Vector (Vector a) -> Vector (Vector a)
transposePar mat =
  vectorFromList $ parMap rpar (\i -> V.map (\row -> row ! i) mat) [0..(V.length $ V.head mat) - 1]

--multiply matrices
multiply :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiply mat1 mat2 =
  let t = transposeMat mat2 in
  V.map (\row -> V.map (dotProduct row) t) mat1

--multiply matrices, evaluate rows in parallel if processors are available
multiplyPar :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiplyPar mat1 mat2 = runEval $ do
  let t = transposeMat mat2
  rseq t
  return $ parMapVec (\row -> V.map (dotProduct row) t) mat1

--gets the absolute value of every diagonal element, absolute value is identity for booleans in this case
getUnsignedDiagonal :: Num a => Vector (Vector a) -> [a]
getUnsignedDiagonal matrix =
  L.map (\i -> abs $ matrix ! i ! i) [0..(min (V.length matrix) (V.length $ V.head matrix)) - 1]

--SMITH NORMAL FORM OF INTEGER MATRICES-----------------------------------

--rearranges the matrix if necessary, returns the matrix paired with its pivot
--first argument is the index of the pivot row
choosePivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
choosePivotInt i mat = --assumes that i is a legal index for mat
  let pos = --Nothing if pivot is found, Right if a column switch needs to be performed, Left otherwise
        let row = mat ! i in
        if row ! i == 0 then
        case V.findIndex (\n -> n /= 0) row of
          Just x  -> Just $ Right (i, x)
          Nothing -> Nothing
        else if exactlyOneNonZero row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (Just $ mat ! x ! x, mat)
    Just (Right (x, y)) ->
      let newElems = V.map (switchElems x y) mat in
      (Just $ newElems ! x ! x, newElems)
    Nothing             -> (Nothing, mat)

--does a column operation specific to Smith Normal Form algo on a single row
--good because parMap can be used to work on whole matrix in parallel
--first argument is the index of the pivot row, the second is the bezout coefficients,
--the quotients of the two elements by their gcd, and the index of the non-pivot element
colOperationHelperInt :: Int -> (Int, Int, Int, Int, Int) -> Vector Int -> Vector Int
colOperationHelperInt pIndex (c1, c2, q1, q2, index) row =
  let elem1 = row ! index; elem2 = row ! pIndex in
  if index < pIndex then
  let first  = V.take index row
      second = V.drop (index + 1) (V.take pIndex row)
      third  = V.drop (pIndex + 1) row in
  first V.++ (cons (q2*elem1 - q1*elem2) second) V.++ (cons (c1*elem2 + c2*elem1) third)
  else
  let first  = V.take pIndex row
      second = V.drop (pIndex + 1) (V.take index row)
      third  = V.drop (index + 1) row in
  first V.++ (cons (c1*elem1 + c2*elem2) second) V.++ (cons (q2*elem1 - q1*elem2) third)

--does necessary rearranging if pivot is found
--returns pivot if it is found with the new matrix
chooseColumnPivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
chooseColumnPivotInt index elems =
  let pos =
        let col = V.map (\row -> row ! index) elems in
        if elems ! index ! index == 0 then
          case V.findIndex (\n -> n /= 0) col of
            Just x  -> Just $ Right (index, x)
            Nothing -> Nothing
        else if exactlyOneNonZero col then Nothing
        else Just $ Left index in
  case pos of
    Just (Left i)       -> (Just $ elems ! i ! i, elems)
    Just (Right (i, j)) ->
      let newElems = switchElems i j elems in
      (Just $ newElems ! i ! i, newElems)
    Nothing             -> (Nothing, elems)
    
--does the same thing as colOperationHelper except with rows, operates on the whole matrix
rowOperationHelper :: Int -> (Int, Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperationHelper pIndex (c1, c2, q1, q2, index) elems =
  let row1 = elems ! index; row2 = elems ! pIndex in
  if index < pIndex then
    let first  = V.take index elems
        second = V.drop (index + 1) (V.take pIndex elems)
        third  = V.drop (pIndex + 1) elems in
    first V.++ (cons ((q2 `mul` row1) `subtr` (q1 `mul` row2)) second)
     V.++ (cons ((c1 `mul` row2) `add` (c2 `mul` row1)) third)
  else
    let first  = V.take pIndex elems
        second = V.drop (pIndex + 1) (V.take index elems)
        third  = V.drop (index + 1) elems in
    first V.++ (cons ((c1 `mul` row1) `add` (c2 `mul` row2)) second)
      V.++ (cons ((q2 `mul` row1) `subtr` (q1 `mul` row2)) third)

--same as rowOperationHelperPar except linear combinations of rows can be computed in parallel
rowOperationHelperPar :: Int -> (Int, Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperationHelperPar pIndex (c1, c2, q1, q2, index) elems =
  let row1 = elems ! index; row2 = elems ! pIndex in
  if index < pIndex then
    let first  = V.take index elems
        second = V.drop (index + 1) (V.take pIndex elems)
        third  = V.drop (pIndex + 1) elems
        res    = runEval $ do
          a <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          b <- rpar $ (c1 `mul` row2) `add` (c2 `mul` row1)
          rseq a
          rseq b
          return (a, b) in
    first V.++ (cons (fst res) second) V.++ (cons (snd res) third)
  else
    let first  = V.take pIndex elems
        second = V.drop (pIndex + 1) (V.take index elems)
        third  = V.drop (index + 1) elems
        res    = runEval $ do
          a <- rpar $ (c1 `mul` row1) `add` (c2 `mul` row2)
          b <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          rseq a
          rseq b
          return (a, b) in
    first V.++ (cons (fst res) second) V.++ (cons (snd res) third)

--given the pivot of a matrix and the matrix itself to improve that row with column operations
--makes every element in the row divisible by the pivot
--returns the new matrix paired with the new pivot
improveRowSmithInt :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveRowSmithInt pIndex (pivot, elems) =
  let row          = elems ! pIndex
      nonDivis     = elemAndIndex (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n --extended Euclidean algorithm, returns gcd tupled with coefficients that combine the args to get the gcd
          gcd          = one gcdTriple
          transform    = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
          newElems     = V.map (colOperationHelperInt pIndex transform) elems in
      improveRowSmithInt pIndex (newElems ! pIndex ! pIndex, newElems)
    Nothing     -> (elems ! pIndex ! pIndex, elems)

--same as above except row operations can be performed in parallel
improveRowSmithIntPar :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveRowSmithIntPar pIndex (pivot, elems) =
    let row          = elems ! pIndex
        nonDivis     = elemAndIndex (\n -> n /= 0 && n `mod` pivot /= 0) row in
    case nonDivis of
      Nothing     -> (elems ! pIndex ! pIndex, elems)
      Just (n, i) ->
        let gcdTriple    = extEucAlg pivot n
            gcd          = one gcdTriple
            transform    = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
            newElems     = parMapVec (colOperationHelperInt pIndex transform) elems in
        improveRowSmithInt pIndex (newElems ! pIndex ! pIndex, newElems)

--given pivot index and pivot paired with matrix whose pivot row has been improved, eliminates the entries in the pivot row
eliminateRowInt :: Int -> (Int, IMatrix) -> IMatrix
eliminateRowInt pIndex (pivot, elems) =
  let row      = elems ! pIndex
      coeffs   = V.map (\n -> if n == 0 then 0 else n `div` pivot) row in
  V.map (\r -> mapWithIndexVec (\i elem -> if i == pIndex then elem else elem - (coeffs ! i)*(row ! pIndex)) r) elems

--same as above except it works on the pivot column
eliminateColInt :: Int -> (Int, IMatrix) -> IMatrix
eliminateColInt pIndex (pivot, elems) =
  let pRow     = elems ! pIndex
      coeffs   = V.map (\row -> let n = row ! pIndex in if n == 0 then 0 else n `div` pivot) elems in
  mapWithIndexVec (\i row -> if i == pIndex then row else row `subtr` ((coeffs ! i) `mul` pRow)) elems

--given pivot index and pivot paired with matrix, improves pivot column with row operations
improveColInt :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveColInt pIndex (pivot, elems) =
  case elemAndIndex (\row -> let n = row ! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
     Nothing     -> (elems ! pIndex ! pIndex, elems)
     Just (r, i) ->
       let n         = r ! pIndex
           gcdTriple = extEucAlg pivot n
           gcd       = one gcdTriple
           transform = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
           newElems  = rowOperationHelper pIndex transform elems in
       improveColInt pIndex (newElems ! pIndex ! pIndex, newElems)

--same as above except it uses rowOperationHelperPar
improveColIntPar :: Int -> (Int, IMatrix) -> (Int, IMatrix)
improveColIntPar pIndex (pivot, elems) =
    case elemAndIndex (\row -> let n = row ! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
    Nothing     -> (elems ! pIndex ! pIndex, elems)
    Just (r, i) ->
        let n         = r ! pIndex
            gcdTriple = extEucAlg pivot n
            gcd       = one gcdTriple
            transform = (two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd, i)
            newElems  = rowOperationHelperPar pIndex transform elems in
        improveColIntPar pIndex (newElems ! pIndex ! pIndex, newElems)

--gets the Smith normal form of an integer matrix
getSmithNormalFormInt :: IMatrix -> IMatrix
getSmithNormalFormInt matrix =
  let maxIndex     = min (V.length matrix) (V.length $ V.head matrix)
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
  let maxIndex     = min (V.length matrix) (V.length $ V.head matrix)
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
        let row = mat ! i in
        if not $ row ! i then
          case V.findIndex id row of
            Just x  -> Just $ Right (i, x)
            Nothing -> Nothing
        else if exactlyOneTrue row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat)
    Just (Right (x, y)) -> (True, V.map (switchElems x y) mat)
    Nothing             -> (False, mat)

--switches rows if necessary to get the value at the pivot position to be 1
chooseColumnPivotBool :: Int -> BMatrix -> (Bool, BMatrix)
chooseColumnPivotBool index elems =
  if elems ! index ! index then (True, elems) else
    case elemAndIndex (\row -> row ! index) elems of
      Nothing     -> (False, elems)
      Just (_, i) -> (True, switchElems i index elems)

--eliminates pivot row of a boolean matrix
elimRowSmithBool :: Int -> BMatrix -> BMatrix
elimRowSmithBool pIndex mat =
  let row          = mat ! pIndex
      indices      = V.elemIndices True row
      calc is m    =
        if V.null is then m else
          let x = V.head is; xs = V.tail is in
          if x == pIndex then calc xs m
          else calc xs $ V.map (\r -> (V.take x r) V.++ (cons ((r ! x) + (r ! pIndex)) (V.drop (x + 1) r))) mat in
  calc indices mat

--eliminates pivot column of a boolean matrix
elimColBool :: Int -> BMatrix -> BMatrix
elimColBool pIndex elems =
  let pRow     = elems ! pIndex in
  mapWithIndexVec (\i row -> if (not $ row ! pIndex) || i == pIndex then row else row `add` pRow) elems

--gets the Smith normal form of a boolean (mod 2) matrix, no parallel version because its very cheap
getSmithNormalFormBool :: BMatrix -> BMatrix
getSmithNormalFormBool matrix =
    let maxIndex     = min (V.length matrix) (V.length $ V.head matrix)
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

--preps the matrix for gauss-jordan elimination
--returns the index of the last non-zero row, the prepped matrix,
--and the identity matrix with the same column operations performed
moveAllZeroColsBackInt :: IMatrix -> IMatrix -> (Int, IMatrix, IMatrix)
moveAllZeroColsBackInt elems identity =
  let len    = (V.length $ V.head elems) - 1
      zeroes = L.filter (\i -> forallVec (\row -> row ! i == 0) elems) [0..len]
      op     = V.map (moveToBack zeroes) in
  (len - (L.length zeroes), op elems, op identity)

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
chooseGaussPivotInt :: Int -> IMatrix -> (Bool, IMatrix, Maybe (Int, Int))
chooseGaussPivotInt i mat = --assumes that i is a legal index for mat
  let row  = mat ! i
      elem = row ! i
      pos  =
        if elem == 0 then
        case L.filter (\index -> index > i) $ L.findIndices (\n -> n /= 0) $ listFromVector row of
          (x:_)  -> Just $ Right (i, x)
          []     -> Nothing
        else if exactlyOneNonZero row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat, Nothing)
    Just (Right (x, y)) ->
      let newElems = V.map (switchElems x y) mat in
      (True, newElems, Just (x, y))
    Nothing             -> (False, mat, Nothing)

--makes every element after the pivot in the pivot row divisible by the pivot
--returns improved matrix (first), new pivot, and the identity with the same column operations performed (third)
improveRowGaussInt :: Int -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowGaussInt pIndex maxIndex elems identity =
  let improve i mat ide =
        if i == maxIndex then (mat, mat ! pIndex ! pIndex, ide) else
          let row   = mat ! pIndex
              pivot = row ! pIndex
              x     = row ! i
              next  = i + 1 in
          if x == 0 || (x `mod` pivot == 0) then
            improve next mat ide else
            let gcdTriple = extEucAlg pivot x
                gcd       = one gcdTriple
                transform = V.map (colOperationHelperInt pIndex (two gcdTriple, thr gcdTriple, pivot `div` gcd, x `div` gcd, i)) in
            improve next (transform mat) (transform ide) in
  improve (pIndex + 1) elems identity

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
eliminateEntriesGaussInt :: Int -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
eliminateEntriesGaussInt pIndex maxIndex (elems, pivot, identity) =
  let row            = elems ! pIndex
      elim i mat ide =
        if i == maxIndex then (mat, ide)
        else 
          let coeff     = (row ! i) `div` pivot
              transform = V.map (\r -> (V.take i r) V.++ (cons ((r ! i) - coeff*(r ! pIndex)) (V.drop (i + 1) r))) in
          elim (i + 1) (transform mat) (transform ide) in
  elim (pIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernelInt :: IMatrix -> IMatrix
findKernelInt matrix =
  let len      = V.length matrix
      len0     = V.length $ V.head matrix
      len01    = len0 - 1
      maxIndex = if len > len0 then len0 else len
      identity = vectorFromList $ L.map (\i -> (V.replicate i 0) V.++ (cons 1 (V.replicate (len01 - i) 0))) [0..len01]
      zeroData = moveAllZeroColsBackInt matrix identity
      doColOps index (elems, ide) =
        if index == (one zeroData) || index == maxIndex then (elems, ide) else 
          case chooseGaussPivotInt index elems of
            (True, _, Nothing)  ->
              doColOps (index + 1) $ eliminateEntriesGaussInt index len0 $ improveRowGaussInt index len0 elems ide
            (True, mx, Just (i, j)) ->
              doColOps (index + 1) $ eliminateEntriesGaussInt index len0 $ improveRowGaussInt index len0 mx $ V.map (switchElems i j) ide
            (False, _, _)       -> doColOps (index + 1) (elems, ide)
      result   = doColOps 0 (not1 zeroData)
      elems    = fst result
      ide      = snd result in
  V.map (\i -> V.map (\row -> row ! i) ide) $ V.filter (\i -> forallVec (\row -> row ! i == 0) elems) $ vectorFromList [0..len01]

--KERNEL OF BOOLEAN MATRICES----------------------------------------------

--preps the matrix for gauss-jordan elimination
--returns the index of the last non-zero row, the prepped matrix (2nd)
--and the identity matrix with the same column operations performed (3rd)
moveAllZeroColsBackBool :: BMatrix -> BMatrix -> (Int, BMatrix, BMatrix)
moveAllZeroColsBackBool elems identity =
  let len    = (V.length $ V.head elems) - 1
      zeroes = L.filter (\i -> forallVec (\row -> not $ row ! i) elems) [0..len]
      op     = V.map (moveToBack zeroes) in
  (len - (L.length zeroes), op elems, op identity)

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
chooseGaussPivotBool :: Int -> BMatrix -> (Bool, BMatrix, Maybe (Int, Int))
chooseGaussPivotBool i mat = --assumes that i is a legal index for mat
  let row  = mat ! i
      elem = row ! i
      pos  =
        if not elem then
        case L.filter (\index -> index > i) $ L.findIndices (\n -> n /= 0) $ listFromVector row of
          (x:_)  -> Just $ Right (i, x)
          []     -> Nothing
        else if exactlyOneTrue row then Nothing
        else Just $ Left i in
  case pos of
    Just (Left x)       -> (True, mat, Nothing)
    Just (Right (x, y)) ->
      let newElems = V.map (switchElems x y) mat in
      (True, newElems, Just (x, y))
    Nothing             -> (False, mat, Nothing)

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
eliminateEntriesGaussBool :: Int -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
eliminateEntriesGaussBool pIndex maxIndex elems identity =
  let row            = elems ! pIndex
      len            = V.length $ V.head elems
      elim i mat ide =
          if i == len then (mat, ide)
          else let transform = V.map (\row -> (V.take i row) V.++ (cons ((row ! i) + (row ! pIndex)) (V.drop (i + 1) row))) in
            elim (i + 1) (transform mat) (transform ide) in
  elim (pIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernelBool :: BMatrix -> BMatrix
findKernelBool matrix =
  let len      = V.length matrix
      len0     = V.length $ V.head matrix
      len01    = len0 - 1
      maxIndex = if len > len0 then len0 else len
      identity = vectorFromList $ L.map (\i -> (V.replicate i False) V.++ (cons True (V.replicate (len01 - i) False))) [0..len01]
      zeroData = moveAllZeroColsBackBool matrix identity
      doColOps index (elems, ide) =
        if index == (one zeroData) || index == maxIndex then (elems, ide) else 
          case chooseGaussPivotBool index elems of
            (True, _, Nothing)  ->
              doColOps (index + 1) $ eliminateEntriesGaussBool index len0 elems ide
            (True, mx, Just (i, j)) ->
              doColOps (index + 1) $ eliminateEntriesGaussBool index len0 mx $ V.map (switchElems i j) ide
            (False, _, _)       -> doColOps (index + 1) (elems, ide)
      result   = doColOps 0 (not1 zeroData)
      elems    = fst result
      ide      = snd result in
  V.map (\i -> V.map (\row -> row ! i) ide) $ V.filter (\i -> forallVec (\row -> not $ row ! i) elems) $ vectorFromList [0..len01]