--TODOS:
--make more parallel row operation stuff
module Matrix
  ( IMatrix
  , BMatrix
  , IPolynomial
  , IPolyMat
  , BPolynomial
  , BPolyMat
  , getDiagonal
  , transposeMat
  , transposePar
  , multiply
  , multiplyPar
  , rankInt
  , rankIntPar
  , normalFormInt
  , normalFormIntPar
  , kernelInt
  , kernelIntPar
  , imgInKerInt
  , imgInKerIntPar
  , rankBool
  , normalFormBool
  , kernelBool
  , imgInKerBool
  ) where

import Util
import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------
Matrices are transformed by iterating through each row and selecting a pivot
The pivot is the diagonal entry of the row, and must be non-zero
If the diagonal entry is non-zero at first, a switch is performed so that it is

Eliminating elements is a slighltly more complicated process since only integer operations are allowed.
First, every element that must be eliminated is made divisible by the pivt using the bezout coefficients
from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used
to eliminate the elements.

Boolean matrices are regular matrices with elements modulo 2, Bool is an instance
of Num here and the instance is given in Util.
-}

{--LAYOUT-----------------------------------------------------------------
"BASIC STUFF" includes type synonyms, transposing matrices, multiplying matrices,
and row/column operations

"INTEGER MATRICES" is divided into "RANK," "NORMAL FORM," "KERNEL" and "IMAGE IN KERNEL"
Each section provides functions for computing their namesake both sequentially and in parallel

The same applies to "BOOLEAN MATRICES"
-}

--BASIC STUFF-------------------------------------------------------------

type IMatrix = Vector (Vector Int)
type BMatrix = Vector (Vector Bool)

transposeMat :: Vector (Vector a) -> Vector (Vector a)
transposeMat mat =
  V.map (\i -> V.map (\row -> row ! i) mat) $ 0 `range` ((V.length $ V.head mat) - 1)

--takes the transpose of a matrix in parallel
transposePar :: Vector (Vector a) -> Vector (Vector a)
transposePar mat =
  parMapVec (\i -> V.map (\row -> row ! i) mat) $ 0 `range` ((V.length $ V.head mat) - 1)

--multiply matrices
multiply :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiply mat1 mat2 =
  let t = transposeMat mat2
  in V.map (\row -> V.map (dotProduct row) t) mat1

--multiply matrices, evaluate rows in parallel if processors are available
multiplyPar :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiplyPar mat1 mat2 = runEval $ do
  let t = transposeMat mat2
  rseq t
  return $ parMapVec (\row -> V.map (dotProduct row) t) mat1

getDiagonal :: Vector (Vector a) -> [a]
getDiagonal matrix =
  if V.null matrix then []
  else L.map (\i -> matrix ! i ! i) [0..(min (V.length matrix) (V.length $ V.head matrix)) - 1]

getUnsignedDiagonal :: Num a => Vector (Vector a) -> [a]
getUnsignedDiagonal matrix =
  if V.null matrix then []
  else L.map (\i -> abs $ matrix ! i ! i) [0..(min (V.length matrix) (V.length $ V.head matrix)) - 1]

--assumes index1 < index2
colOperation :: Int -> Int -> (Int, Int, Int, Int) -> IMatrix -> IMatrix
colOperation index1 index2 (c11, c12, c21, c22) matrix =
  let calc row =
        let elem1  = row ! index1
            elem2  = row ! index2
            first  = V.take index1 row
            second = V.drop (index1 + 1) (V.take index2 row)
            third  = V.drop (index2 + 1) row
        in first V.++ (cons (c11*elem1 + c12*elem2) second) V.++ (cons (c22*elem2 + c21*elem1) third)
  in V.map calc matrix

colOperationPar :: Int -> Int -> (Int, Int, Int, Int) -> IMatrix -> IMatrix
colOperationPar index1 index2 (c11, c12, c21, c22) matrix =
  let calc row =
        let elem1  = row ! index1
            elem2  = row ! index2
            first  = V.take index1 row
            second = V.drop (index1 + 1) (V.take index2 row)
            third  = V.drop (index2 + 1) row
        in first V.++ (cons (c11*elem1 + c12*elem2) second) V.++ (cons (c22*elem2 + c21*elem1) third)
  in parMapVec calc matrix

--assumes index1 < index2
rowOperation :: Int -> Int -> (Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperation index1 index2 (c11, c12, c21, c22) matrix =
  let row1   = matrix ! index1
      row2 = matrix ! index2
      first  = V.take index1 matrix
      second = V.drop (index1 + 1) $ V.take index2 matrix
      third  = V.drop (index2 + 1) matrix
  in first V.++ (cons ((c11 `mul` row1) `add` (c12 `mul` row2)) second)
    V.++ (cons ((c22 `mul` row2) `add` (c21 `mul` row1)) third)

rowOperationPar :: Int -> Int -> (Int, Int, Int, Int) -> IMatrix -> IMatrix
rowOperationPar index1 index2 (c11, c12, c21, c22) matrix =
  let row1   = matrix ! index1
      row2 = matrix ! index2
      first  = V.take index1 matrix
      second = V.drop (index1 + 1) (V.take index2 matrix)
      third  = V.drop (index2 + 1) matrix
  in runEval $ do
     a <- rpar $ (c11 `mul` row1) `add` (c12 `mul` row2)
     b <- rpar $ (c21 `mul` row1) `add` (c22 `mul` row2)
     rseq (a,b)
     return $ first V.++ (a `cons` second) V.++ (b `cons` third)

--INTEGER MATRICES--------------------------------------------------------

--RANK--------------------------------------------------------------------

--finds the pivot in a given row for Gaussian elimination
--given the index of the pivot row and the matrix
--returns whether or not the row needs to be eliminated with
--the rearranged matrix and
--the column switch performed (if there was one)
--returns Nothing if the row is all zeroes
chooseGaussPivotInt :: (Int, Int) -> IMatrix -> Maybe (Bool, IMatrix, Maybe (Int, Int))
chooseGaussPivotInt (rowIndex, colIndex) mat = --assumes that i is a legal index for mat
  let row     = mat ! rowIndex
      elem    = row ! colIndex
      indices = V.filter (\index -> index >= colIndex) $ V.findIndices (\n -> n /= 0) row
      len     = V.length indices
        in case len of
          0                           -> Nothing
          1 | indices ! 0 == colIndex -> Just (False, mat, Nothing)
          1                           -> Just (False, V.map (switchElems colIndex (indices ! 0)) mat, Just (colIndex, indices ! 0))
          _                           -> Just (True, V.map (switchElems colIndex (indices ! 1)) mat, Just (colIndex, indices ! 1))

--does gaussian elimination on the pivot row of an integer matrix
improveRowInt :: (Int, Int) -> Int -> IMatrix -> IMatrix
improveRowInt (rowIndex, colIndex) numCols matrix =
  let improve i mat =
        if i == numCols then mat else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in --boundary operators have lots of zeroes, better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next $ colOperation colIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
  in improve (colIndex + 1) matrix

--given pivot index and pivot paired with matrix whose pivot row has been improved, eliminates the entries in the pivot row
--the kinds of matrices that the functions work on will have lots of zeroes
--better to catch that with a condition than perform an unnecessary division
elimRowInt :: (Int, Int) -> IMatrix -> IMatrix
elimRowInt (rowIndex, colIndex) elems =
  let pCol  = V.map (\row -> row ! colIndex) elems
      pivot = pCol ! rowIndex
      c1    = colIndex + 1

      makeCoeffs i v =
        if V.null v then empty
        else let x = V.head v; xs = V.tail v in
          if x == 0 then makeCoeffs (i + 1) xs
          else (i, x `div` pivot) `cons` (makeCoeffs (i + 1) xs)

      calc :: IMatrix -> Vector (Int, Int) -> IMatrix
      calc mat ops =
        if V.null ops then mat
        else let (i, coeff) = V.head ops in
          calc (mapWithIndex (\j row -> replaceElem i ((row ! i) - coeff*(pCol ! j)) row) mat) (V.tail ops)

  in calc elems $ makeCoeffs c1 $ V.drop c1 $ elems ! rowIndex

--finds the rank of integer matrix (number of linearly independent columns)
--found by transforming to column eschelon form and counting non-zero cols
rankInt :: IMatrix -> Int
rankInt matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat else 
          case chooseGaussPivotInt (rowIndex, colIndex) mat of
            Just (True, mx, _)  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowInt (rowIndex, colIndex) $ improveRowInt (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mx
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ V.map (\i -> if exists (\j -> mat ! j ! i /= 0) $
          0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

  in countNonZeroCols $ doColOps (0, 0) matrix

--does gaussian elimination on the pivot row of an integer matrix in parallel
improveRowIntPar :: (Int, Int) -> Int -> IMatrix -> IMatrix
improveRowIntPar (rowIndex, colIndex) numCols matrix =
  let improve i mat =
        if i == numCols then mat else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in --boundary operators have lots of zeroes, better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next $ colOperationPar colIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
  in improve (colIndex + 1) matrix

--eliminates a row in parallel
elimRowIntPar :: (Int, Int) -> IMatrix -> IMatrix
elimRowIntPar (rowIndex, colIndex) elems =
  let pCol  = V.map (\row -> row ! colIndex) elems
      pivot = pCol ! rowIndex
      c1    = colIndex + 1

      makeCoeffs i v =
        if V.null v then empty
        else let x = V.head v; xs = V.tail v in
          if x == 0 then makeCoeffs (i + 1) xs
          else (i, x `div` pivot) `cons` (makeCoeffs (i + 1) xs)

      calc :: IMatrix -> Vector (Int, Int) -> IMatrix
      calc mat ops =
        if V.null ops then mat
        else let (i, coeff) = V.head ops in
          calc (parMapWithIndex (\j row -> replaceElem i ((row ! i) - coeff*(pCol ! j)) row) mat) (V.tail ops)

  in calc elems $ makeCoeffs c1 $ V.drop c1 $ elems ! rowIndex

rankIntPar :: IMatrix -> Int
rankIntPar matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat else 
          case chooseGaussPivotInt (rowIndex, colIndex) mat of
            Just (True, mx, _)  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntPar (rowIndex, colIndex) $
                improveRowIntPar (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mx
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ parMapVec (\i -> if exists (\j -> mat ! j ! i /= 0) $
          0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

  in countNonZeroCols $ doColOps (0, 0) matrix

--NORMAL FORM-------------------------------------------------------------

--rearranges matrix so that the pivot entry is in the correct position, returns true if more elimination is necessary
--If the process runs into a zero column and zero row that intersect along the diagonal, it will return Nothing, which is a problem
--if the pivot is the only non-zero entry it returns Just (Nothing, matrix) and Just (Just pivot, matrix) otherwise
chooseRowPivotInt :: Int -> IMatrix -> (Bool, IMatrix)
chooseRowPivotInt i mat =
  let row = mat ! i
  in
    if row ! i == 0 then
      case V.findIndex (\n -> n /= 0) row of
        Just x  -> (True, V.map (switchElems x i) mat)
        Nothing ->
          case V.findIndex (\n -> n /= 0) $ V.map (\row -> row !  i) mat of
            Just x  ->
              if exactlyOneNonZero $ mat ! x then
                (False, switchElems x i mat)
              else (True, switchElems x i mat)
            Nothing -> (False, mat) --problem, see comment above
    else if exactlyOneNonZero row then (False, mat)
    else (True, mat)

--given pivot index and pivot paired with matrix, improves pivot column with row operations
improveColInt :: Int -> Int -> IMatrix -> IMatrix
improveColInt pIndex maxIndex matrix =
  let improve i mat =
        if i == maxIndex then mat else
          let col   = V.map (\row -> row ! pIndex) mat
              pivot = col ! pIndex
              x     = col ! i
              next  = i + 1
          in --boundary operators have lots of zeroes, better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next $ rowOperation pIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
  in improve (pIndex + 1) matrix

--eliminates the pivot column of a matrix to obtain normal form
elimColInt :: Int -> IMatrix -> IMatrix
elimColInt pIndex elems =
  let pRow  = elems ! pIndex
      pivot = pRow ! pIndex
      p1    = pIndex + 1
      makeCoeffs i v =
        if V.null v then empty
        else let x = V.head v; xs = V.tail v in
          if x == 0 then makeCoeffs (i + 1) xs
          else (i, x `div` pivot) `cons` (makeCoeffs (i + 1) xs)
      calc :: IMatrix -> Vector (Int, Int) -> IMatrix
      calc mat ops =
        if V.null ops then mat
        else let (i, coeff) = V.head ops in
          calc (replaceElem i ((mat ! i) `subtr` (coeff `mul` pRow)) mat) (V.tail ops)
  in calc elems $ makeCoeffs p1 $ V.drop p1 $ V.map (\row -> row ! pIndex) elems

 --gets the Smith normal form of an integer matrix
normalFormInt :: IMatrix -> IMatrix
normalFormInt matrix =
  let rows       = V.length matrix
      cols       = V.length $ V.head matrix

      calc (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat
        else
          case chooseRowPivotInt rowIndex mat of
            (True, mx)  ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColInt rowIndex $ improveColInt rowIndex rows $
                  elimRowInt (rowIndex, colIndex) $ improveRowInt (rowIndex, colIndex) cols mx
            (False, mx) ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColInt rowIndex $ improveColInt rowIndex rows mx

  in if V.null matrix then empty else calc (0, 0) matrix

--improves the pivot column of a matrix in parallel
improveColIntPar :: Int -> Int -> IMatrix -> IMatrix
improveColIntPar pIndex maxIndex matrix =
  let improve i mat =
        if i == maxIndex then mat else
          let col   = V.map (\row -> row ! pIndex) mat
              pivot = col ! pIndex
              x     = col ! i
              next  = i + 1
          in --boundary operators have lots of zeroes, better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next $ rowOperationPar pIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
  in improve (pIndex + 1) matrix

--NEEDS TO BE PARALLELIZED
--eliminates pivot column in parallel
elimColIntPar :: Int -> IMatrix -> IMatrix
elimColIntPar pIndex elems =
  let pRow  = elems ! pIndex
      pivot = pRow ! pIndex
      p1    = pIndex + 1
      makeCoeffs i v =
        if V.null v then empty
        else let x = V.head v; xs = V.tail v in
          if x == 0 then makeCoeffs (i + 1) xs
          else (i, x `div` pivot) `cons` (makeCoeffs (i + 1) xs)
      calc :: IMatrix -> Vector (Int, Int) -> IMatrix
      calc mat ops =
        if V.null ops then mat
        else let (i, coeff) = V.head ops in
          calc (replaceElem i ((mat ! i) `subtr` (coeff `mul` pRow)) mat) (V.tail ops)
  in calc elems $ makeCoeffs p1 $ V.drop p1 $ V.map (\row -> row ! pIndex) elems

--gets the Smith normal form of a matrix, uses lots of parallelism if possible
normalFormIntPar :: IMatrix -> IMatrix
normalFormIntPar matrix =
  let rows       = V.length matrix
      cols       = V.length $ V.head matrix

      calc (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat
        else
          case chooseRowPivotInt rowIndex mat of
            (True, mx)  ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColIntPar rowIndex $ improveColIntPar rowIndex rows $
                  elimRowIntPar (rowIndex, colIndex) $ improveRowIntPar (rowIndex, colIndex) cols mx
            (False, mx) ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColIntPar rowIndex $ improveColIntPar rowIndex rows mx

  in if V.null matrix then empty else calc (0, 0) matrix

--KERNEL------------------------------------------------------------------

--improves the pivot row of an integer matrix
--performs the same column operations on the identity
improveRowIntWithId :: (Int, Int) -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowIntWithId (rowIndex, colIndex) numCols elems identity =
  let improve i mat ide =
        if i == numCols then (mat, mat ! rowIndex ! colIndex, ide) else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat ide
            else
            let gcdTriple = extEucAlg pivot x
                gcd       = one gcdTriple
                transform = colOperation colIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd))
            in improve next (transform mat) (transform ide)
  in improve (colIndex + 1) elems identity

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
elimRowIntWithId :: (Int, Int) -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
elimRowIntWithId (rowIndex, colIndex) numCols (elems, pivot, identity) =
  let row            = elems ! rowIndex
      elim i mat ide =
        if i == numCols then (mat, ide)
        else
          let coeff     = (row ! i) `div` pivot
              transform = V.map (\r -> (V.take i r) V.++ (cons ((r ! i) - coeff*(r ! colIndex)) (V.drop (i + 1) r)))
          in elim (i + 1) (transform mat) (transform ide)
  in elim (colIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
kernelInt :: IMatrix -> IMatrix
kernelInt matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i 0) V.++ (cons 1 (V.replicate (cols1 - i) 0))) $ 0 `range` cols1

      doColOps (rowIndex, colIndex) (elems, ide) =
        if rowIndex == rows || colIndex == cols then (elems, ide) else 
          case chooseGaussPivotInt (rowIndex, colIndex) elems of
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithId (rowIndex, colIndex) cols $
                improveRowIntWithId (rowIndex, colIndex) cols mx $ V.map (switchElems i j) ide
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithId (rowIndex, colIndex) cols $
                improveRowIntWithId (rowIndex, colIndex) cols elems ide
            Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, V.map (switchElems i j) ide)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (elems, ide)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (elems, ide)

      result   = doColOps (0, 0) (matrix, identity)
      elems    = fst result
      ide      = snd result
  in V.map (\i -> V.map (\row -> row ! i) ide) $ V.filter (\i -> forallVec (\row -> row ! i == 0) elems) $ 0 `range` cols1

--improves row in parallel and does the same thing to the identity matrix in parallel
improveRowIntWithIdPar :: (Int, Int) -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowIntWithIdPar (rowIndex, colIndex) numCols elems identity =
  let improve i mat ide =
        if i == numCols then (mat, mat ! rowIndex ! colIndex, ide) else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in --boundary operators have lots of zeroes, better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat ide
            else
            let gcdTriple = extEucAlg pivot x
                gcd       = one gcdTriple
                transform = colOperationPar colIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd))
            in improve next (transform mat) (transform ide)
  in improve (colIndex + 1) elems identity

--eliminates entries in the pivot row in parallel and does the same to the identity
elimRowIntWithIdPar :: (Int, Int) -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
elimRowIntWithIdPar (rowIndex, colIndex) numCols (elems, pivot, identity) =
  let row            = elems ! rowIndex
      elim i mat ide =
        if i == numCols then (mat, ide)
        else
          let coeff     = (row ! i) `div` pivot
              transform = parMapVec (\r -> (V.take i r) V.++ (cons ((r ! i) - coeff*(r ! colIndex)) (V.drop (i + 1) r)))
          in elim (i + 1) (transform mat) (transform ide)
  in elim (colIndex + 1) elems identity

--computes kernel of an integer matrix in parallel
kernelIntPar :: IMatrix -> IMatrix
kernelIntPar matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i 0) V.++ (cons 1 (V.replicate (cols1 - i) 0))) $ 0 `range` cols1

      doColOps (rowIndex, colIndex) (elems, ide) =
        if rowIndex == rows || colIndex == cols then (elems, ide) else 
          case chooseGaussPivotInt (rowIndex, colIndex) elems of
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithIdPar (rowIndex, colIndex) cols $
                improveRowIntWithIdPar (rowIndex, colIndex) cols mx $ V.map (switchElems i j) ide
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithIdPar (rowIndex, colIndex) cols $
                improveRowIntWithIdPar (rowIndex, colIndex) cols elems ide
            Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, V.map (switchElems i j) ide)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (elems, ide)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (elems, ide)

      result   = doColOps (0, 0) (matrix, identity)
      elems    = fst result
      ide      = snd result
  in V.map (\i -> V.map (\row -> row ! i) ide) $ V.filter (\i -> forallVec (\row -> row ! i == 0) elems) $ 0 `range` cols1

--FIND IMAGE IN BASIS OF KERNEL-------------------------------------------

--improves the row of the first matrix
--and performs the inverse column operations on the second matrix
improveRowIntWithInv :: (Int, Int) -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowIntWithInv (rowIndex, colIndex) numCols kernel image =
  let improve i ker img =
        if i == numCols then (ker, ker ! rowIndex ! colIndex, img) else
          let row   = ker ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in
            if x == 0 || (x `mod` pivot == 0) then
              improve next ker img
            else
              let gcdTriple  = extEucAlg pivot x
                  gcd        = one gcdTriple
                  q1         = pivot `div` gcd
                  q2         = x `div` gcd
                  transform1 = colOperationPar colIndex i (thr gcdTriple, two gcdTriple, q2, -q1)
                  transform2 = rowOperationPar colIndex i (-q1, -(two gcdTriple), -q2, thr gcdTriple)
              in improve next (transform1 ker) (transform2 img)
  in improve (colIndex + 1) kernel image

--eliminates the row if the first matrix
--performs inverse column operations on the second matrix
elimRowIntWithInv :: (Int, Int) -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
elimRowIntWithInv (rowIndex, colIndex) numCols (kernel, pivot, image) =
  let elim i ker img
        | i == numCols            = (ker, img)
        | ker ! rowIndex ! i == 0 = elim (i + 1) ker img
        | otherwise               =
          let coeff      = (ker ! rowIndex ! i) `div` pivot
              transform1 = V.map (\r -> replaceElem i ((r ! i) - coeff*(r ! colIndex)) r)
              transform2 = \mat -> replaceElem i ((mat ! i) `add` (coeff `mul` (mat ! colIndex))) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
  in elim (colIndex + 1) kernel image

--calculates the image of the second matrix represented in the basis of the kernel of the first matrix
imgInKerInt :: IMatrix -> IMatrix -> IMatrix
imgInKerInt toColEsch toImage =
  let rows     = V.length toColEsch
      cols     = V.length $ V.head toColEsch

      doColOps (rowIndex, colIndex) (ker, img) =
        if rowIndex == rows || colIndex == cols then (ker, img)
        else case chooseGaussPivotInt (rowIndex, colIndex) ker of
          Just (True, _, Nothing)       ->
            doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithInv (rowIndex, colIndex) cols $
              improveRowIntWithInv (rowIndex, colIndex) cols ker img
          Just (True, mx, Just (i, j))  ->
            doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithInv (rowIndex, colIndex) cols $
              improveRowIntWithInv (rowIndex, colIndex) cols mx $ switchElems i j img
          Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
          Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
          Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result = doColOps (0, 0) (toColEsch, toImage)
      ker    = fst result
      img    = snd result
  in V.map (\i -> img ! i) $ V.filter (\i -> forallVec (\row -> row ! i == 0) ker) $ 0 `range` (cols - 1)

--improves row and does inverse operations in parallel
improveRowIntWithInvPar :: (Int, Int) -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowIntWithInvPar (rowIndex, colIndex) numCols kernel image =
  let improve i ker img =
        if i == numCols then (ker, ker ! rowIndex ! colIndex, img) else
          let row   = ker ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in
            if x == 0 || (x `mod` pivot == 0) then
              improve next ker img
            else
              let gcdTriple  = extEucAlg pivot x
                  gcd        = one gcdTriple
                  q1         = pivot `div` gcd
                  q2         = x `div` gcd
                  transform1 = colOperationPar colIndex i (thr gcdTriple, two gcdTriple, q2, -q1)
                  transform2 = rowOperationPar colIndex i (-q1, -(two gcdTriple), -q2, thr gcdTriple)
              in improve next (transform1 ker) (transform2 img)
  in improve (colIndex + 1) kernel image

--eliminates row in parallel
--INVERSE OPERATIONS NEED TO BE PARALLELIZED
elimRowIntWithInvPar :: (Int, Int) -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
elimRowIntWithInvPar (rowIndex, colIndex) numCols (kernel, pivot, image) =
  let elim i ker img
        | i == numCols            = (ker, img)
        | ker ! rowIndex ! i == 0 = elim (i + 1) ker img
        | otherwise               =
          let coeff      = (ker ! rowIndex ! i) `div` pivot
              transform1 = parMapVec (\r -> replaceElem i ((r ! i) - coeff*(r ! colIndex)) r)
              transform2 = \mat -> replaceElem i ((mat ! i) `add` (coeff `mul` (mat ! colIndex))) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
  in elim (colIndex + 1) kernel image

imgInKerIntPar :: IMatrix -> IMatrix -> IMatrix
imgInKerIntPar toColEsch toImage =
  let rows     = V.length toColEsch
      cols     = V.length $ V.head toColEsch

      doColOps (rowIndex, colIndex) (ker, img) =
        if rowIndex == rows || colIndex == cols then (ker, img)
        else case chooseGaussPivotInt (rowIndex, colIndex) ker of
          Just (True, _, Nothing)       ->
            doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithInvPar (rowIndex, colIndex) cols $
              improveRowIntWithInvPar (rowIndex, colIndex) cols ker img
          Just (True, mx, Just (i, j))  ->
            doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithInvPar (rowIndex, colIndex) cols $
              improveRowIntWithInvPar (rowIndex, colIndex) cols mx $ switchElems i j img
          Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
          Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
          Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result = doColOps (0, 0) (toColEsch, toImage)
      ker    = fst result
      img    = snd result
  in V.map (\i -> img ! i) $ V.filter (\i -> forallVec (\row -> row ! i == 0) ker) $ 0 `range` (cols - 1)

--BOOLEAN MATRICES--------------------------------------------------------

--RANK--------------------------------------------------------------------

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
--returns Nothing if the entire row is zero
chooseGaussPivotBool :: (Int, Int) -> BMatrix -> Maybe (Bool, BMatrix, Maybe (Int, Int))
chooseGaussPivotBool (rowIndex, colIndex) mat =
  let row  = mat ! rowIndex
      elem = row ! colIndex
  in
    if not elem then
      case V.filter (\index -> index > colIndex) $ V.findIndices id row of
        v | V.null v -> Nothing
        v            ->
          let j = V.head v
          in
            if exactlyOneTrue row then Just (False, V.map (switchElems colIndex j) mat, Just (colIndex, j))
            else Just (True, V.map (switchElems colIndex j) mat, Just (colIndex, j))
    else if exactlyOneTrue row then Just (False, mat, Nothing)
    else Just (True, mat, Nothing)

--eliminates pivot row of a boolean matrix
elimRowBool :: (Int, Int) -> Int -> BMatrix -> BMatrix
elimRowBool (rowIndex, colIndex) numCols elems =
  let row = elems ! rowIndex
      elim i mat
        | i == numCols  = mat
        | not $ row ! i = elim (i + 1) mat
        | otherwise     = elim (i + 1) $ V.map (\row -> replaceElem i ((row ! i) + (row ! colIndex)) row) mat
  in elim (colIndex + 1) elems

--find the rank of a mod 2 matrix
rankBool :: BMatrix -> Int
rankBool matrix =
  let rows  = V.length matrix
      cols  = V.length $ V.head matrix
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat else
          case chooseGaussPivotBool (rowIndex, colIndex) mat of --FIX
            Just (True, mx, _)  -> doColOps (rowIndex + 1, colIndex + 1) $ elimRowBool (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mat
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ V.map (\i -> if exists (\j -> mat ! j ! i /= 0) (0 `range` (rows - 1)) then 1 else 0) $ 0 `range` cols1
  in countNonZeroCols $ doColOps (0, 0) matrix

--NORMAL FORM-------------------------------------------------------------

--rearranges the matrix if necessary, returns the matrix paired with its pivot
--first argument is the index of the pivot row
chooseRowPivotBool :: Int -> BMatrix -> (Bool, BMatrix)
chooseRowPivotBool i mat =
  let row = mat ! i
  in
    if not $ row ! i then
      case V.findIndex id row of
        Just x  -> (True, V.map (switchElems x i) mat)
        Nothing ->
          case V.findIndex id $ V.map (\row -> row !  i) mat of
            Just x  ->
              if exactlyOneTrue $ mat ! x then
                (False, switchElems x i mat)
              else (True, switchElems x i mat)
            Nothing -> (False, mat) --problem
    else if exactlyOneTrue row then (False, mat)
    else (True, mat)

--eliminates pivot column of a boolean matrix
elimColBool :: (Int, Int) -> Int -> BMatrix -> BMatrix
elimColBool (rowIndex, colIndex) numRows elems =
  let col = V.map (\r -> r ! colIndex) elems
      elim i mat
        | i == numRows  = mat
        | not $ col ! i = elim (i + 1) mat
        | otherwise     = elim (i + 1) $ replaceElem i ((mat ! i) `add` (mat ! rowIndex)) mat
  in elim (rowIndex + 1) elems

--gets the Smith normal form of a boolean (mod 2) matrix, no parallel version because its very cheap
normalFormBool :: BMatrix -> BMatrix
normalFormBool matrix =
  let rows = V.length matrix
      cols = V.length $ V.head matrix
      calc (rowIndex, colIndex) mat =
          if rowIndex == rows || colIndex == cols then mat else
          case chooseRowPivotBool rowIndex mat of
              (False, new)  ->
                calc (rowIndex + 1, colIndex + 1) $ elimColBool (rowIndex, colIndex) rows new
              (True, new)   ->
                calc (rowIndex + 1, colIndex + 1) $ elimColBool (rowIndex, colIndex) rows $
                  elimRowBool (rowIndex, colIndex) cols new
  in if V.null matrix then empty else calc (0, 0) matrix

--KERNEL------------------------------------------------------------------

--eliminates all the entries in the pivot row that come after the pivot, after the matrix has been improved
--returns the new matrix (fst) paired with the identity with whatever column operations were performed (snd)
elimRowBoolWithId :: (Int, Int) -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
elimRowBoolWithId (rowIndex, colIndex) numCols elems identity =
  let row = elems ! rowIndex
      elim i mat ide
        | i == numCols  = (mat, ide)
        | not $ row ! i = elim (i + 1) mat ide
        | otherwise     =
          let transform = V.map (\row -> replaceElem i ((row ! i) + (row ! colIndex)) row)
          in elim (i + 1) (transform mat) (transform ide)
  in elim (colIndex + 1) elems identity

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
kernelBool :: BMatrix -> BMatrix
kernelBool matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i False) V.++ (cons True (V.replicate (cols1 - i) False))) $ 0 `range` cols1

      doColOps (rowIndex, colIndex) (ker, ide) =
        if rowIndex == rows || colIndex == cols then (ker, ide)
        else 
          case chooseGaussPivotBool (rowIndex, colIndex) ker of
            Just (True, _, Nothing)      ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithId (rowIndex, colIndex) cols ker ide
            Just (True, mx, Just (i, j)) ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithId (rowIndex, colIndex) cols mx $ V.map (switchElems i j) ide
            Just (False, _, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (ker, V.map (switchElems i j) ide)
            Just (False, _, _)           -> doColOps (rowIndex + 1, colIndex + 1) (ker, ide)
            Nothing                      -> doColOps (rowIndex + 1, colIndex) (ker, ide)

      result = doColOps (0, 0) (matrix, identity)
      ker    = fst result
      img    = snd result
  in V.map (\i -> V.map (\row -> row ! i) img) $ V.filter (\i -> forallVec (\row -> not $ row ! i) ker) $ 0 `range` cols1

--IMAGE IN BASIS OF KERNEL------------------------------------------------

elimRowBoolWithInv :: (Int, Int) -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
elimRowBoolWithInv (rowIndex, colIndex) numCols kernel image =
  let row = kernel ! rowIndex
      elim i ker img
          | i == numCols  = (ker, img)
          | not $ row ! i = elim (i + 1) ker img
          | otherwise     =
            let transform1 = V.map (\r -> replaceElem i ((r ! i) + (r ! colIndex)) r)
                transform2 = \mat -> replaceElem i ((mat ! i) `add` (mat ! colIndex)) mat
            in elim (i + 1) (transform1 ker) (transform2 img)
  in elim (colIndex + 1) kernel image

imgInKerBool :: BMatrix -> BMatrix -> BMatrix
imgInKerBool kernel image =
  let rows  = V.length kernel
      cols  = V.length $ V.head kernel
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) (ker, img) =
        if rowIndex == rows || colIndex == cols then (ker, img)
        else
          case chooseGaussPivotBool (rowIndex, colIndex) ker of
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithInv (rowIndex, colIndex) cols ker img
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithInv (rowIndex, colIndex) cols mx $ switchElems i j img
            Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result = doColOps (0, 0) (kernel, image)
      ker    = fst result
      img    = snd result
  in V.map (\i -> V.map (\row -> row ! i) img) $ V.filter (\i -> forallVec (\row -> not $ row ! i) ker) $ 0 `range` cols1

--POLYNOMIALS-------------------------------------------------------------

type IPolynomial = Vector Int
type IPolyMat    = Vector (Vector IPolynomial)

(##) :: IPolynomial -> IPolynomial -> IPolynomial
(##) = \p q ->
  if V.null p then
    if V.null q then V.empty
    else q
  else if V.null q then p
  else ((V.head p) + (V.head q)) `cons` ((V.tail p) ## (V.tail q))

(***) :: IPolynomial -> IPolynomial -> IPolynomial
(***) = \p q ->
  let shift i r = (V.replicate i 0) V.++ r
  in V.foldl1 (##) $ mapWithIndex (\i x -> shift i $ V.map (*x) p) q

type BPolynomial = Int
type BPolyMat    = Vector (Vector BPolynomial)

d :: BPolynomial -> BPolynomial -> BPolynomial
n `d` m =
  if m > n then error "Tried to divide monomial by a monomial of a higher degree, the result would not be a polynomial."
  else n - m

m :: BPolynomial -> BPolynomial -> BPolynomial
n `m` m = (n + m)