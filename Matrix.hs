--TODOS:
--make parallelism better
module Matrix
  ( getDiagonal
  , getUnsignedDiagonal
  , transposeMat
  , transposePar
  , multiply
  , multiplyPar
  , IMatrix
  , rankInt
  , rankIntPar
  , normalFormInt
  , normalFormIntPar
  , kernelInt
  , kernelIntPar
  , imgInKerInt
  , imgInKerIntPar
  , BMatrix
  , rankBool
  , normalFormBool
  , kernelBool
  , imgInKerBool
  , IPolynomial
  , IPolyMat
  , BMonomial (Power, Zero)
  , BPolyMat
  , eschelonFormBool
  , eschelonAndNextBool
  ) where

import Util
import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------

Matrices are transformed by iterating through each row and selecting a pivot. Zero rows are skipped for finding column eschelon form but a row operation is performed (if possible) if there is a zero row for Smith normal form.

To get the smith normal form, the entire pivot row and column is eliminated before continuing

To get column eschelon form, every element in the pivot row after the pivot is eliminated. To get the kernel, all column operations to get the matrix to this form are also performed on the identiy matrix. To get the image of one matrix inside the kernel of the one being put into column eschelon form, perform the inverse operations on the matrix whose image is needed. See second paper.

To get the rank of a matrix, look at the number of non-zero columns in the column eschelon form. To get the kernel, look at the columns of the identity (after all of the same column operations have been performed on it) which correspond to zero columns of the column eschelon form.

Eliminating elements is a slighltly more complicated process since only integer operations are allowed. First, every element that must be eliminated is made divisible by the pivt using the bezout coefficients from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used to eliminate the elements.

Boolean matrices are regular matrices with elements modulo 2, Bool is an instance of Num here and the instance is given in Util.

--LAYOUT-----------------------------------------------------------------

"BASIC STUFF" includes type synonyms, transposing matrices, multiplying matrices, and row/column operations.

"INTEGER MATRICES" is divided into "RANK," "NORMAL FORM," "KERNEL" and "IMAGE IN KERNEL." Each section provides functions for computing their namesake both sequentially and in parallel.

The same applies to "BOOLEAN MATRICES"

"INTEGER POLYNOMIALS" and "MOD 2 POLYNOMIALS" contain functions for finding the image of one matrix inside the kernel of the other where both matrices are over the respective polynomial rings.
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

--finds the pivot in a given row for Gaussian elimination given the index of the pivot row and the matrix
--returns whether or not the row needs to be eliminated with the rearranged matrix and
--the column switch performed (if there was one), returns Nothing if the row is all zeroes
chooseGaussPivotInt :: (Int, Int) -> IMatrix -> Maybe (Bool, IMatrix, Maybe (Int, Int))
chooseGaussPivotInt (rowIndex, colIndex) mat = --assumes that i is a legal index for mat
  let row     = mat ! rowIndex --the following variable should be useful for quickly determining whether or not there are more entries to eleiminate
      indices = V.filter (\index -> index > colIndex) $ V.findIndices (\x -> x /= 0) row --but that method is not working for some reason
  in
    if row ! colIndex == 0 then
      case indices of
        v | V.null v -> Nothing
        v            ->
          let j = V.head v
          in Just (not $ exactlyOneNonZero row, V.map (switchElems colIndex j) mat, Just (colIndex, j))
    else Just (not $ exactlyOneNonZero row, mat, Nothing)

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
        V.sum $ V.map (\i -> if existsVec (\j -> mat ! j ! i /= 0) $ 0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

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
        V.sum $ parMapVec (\i -> if existsVec (\j -> mat ! j ! i /= 0) $
          0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

  in countNonZeroCols $ doColOps (0, 0) matrix

--NORMAL FORM-------------------------------------------------------------

--rearranges matrix so that the pivot entry is in the correct position, returns true if more elimination is necessary
--returns Nothing if there is nothing but zeroes after the current pivot position
chooseRowPivotInt :: (Int, Int) -> Int -> Int -> IMatrix -> Maybe (Bool, IMatrix)
chooseRowPivotInt (rowIndex, colIndex) numRows numCols mat =
  let row      = mat ! rowIndex
      rIndices = V.toList $ V.findIndices (\x -> x /= 0) row
  in
    if 0 == row ! colIndex then
      case rIndices of
        (i:is)  -> Just ((L.length is) > 0, V.map (switchElems i colIndex) mat)
        []      ->
          case V.toList $ V.findIndices (\x -> x /= 0) $ V.map (\r -> r ! colIndex) mat of
            (i:_)  -> Just (True, switchElems i rowIndex mat)
            []     -> Nothing
    else Just ((L.length rIndices) > 1, mat)

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
elimColInt :: (Int, Int) -> IMatrix -> IMatrix
elimColInt (rowIndex, colIndex) elems =
  let pRow  = elems ! rowIndex
      pivot = pRow ! colIndex
      ri1   = rowIndex + 1
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
  in calc elems $ makeCoeffs ri1 $ V.drop ri1 $ V.map (\row -> row ! colIndex) elems

finish :: Int -> IMatrix -> IMatrix
finish diagLen matrix =
  let calc i mat =
        let i1    = i + 1
            row   = mat ! i
            entry = row ! i
            nextR = mat ! i1
            nextE = nextR ! i1
        in
          if entry == 0 || i1 == diagLen then mat
          else if entry `divides` nextE then calc i1 mat
          else
            let mat'      = replaceElem i (replaceElem i1 nextE row) mat
                gcdTriple = extEucAlg entry nextE; gcd = one gcdTriple
                improve   = colOperation i i1 (thr gcdTriple, two gcdTriple, -(nextE `div` gcd), entry `div` gcd)
                cleanup   = \m -> elimColInt (i, i) $ elimRowInt (i, i) m
            in calc i1 $ cleanup $ improve mat'
      filtered = biFilter (\row -> existsVec (\x -> x /= 0) row) matrix
  in calc 0 $ (fst filtered) V.++ (snd filtered)

 --gets the Smith normal form of an integer matrix
normalFormInt :: IMatrix -> IMatrix
normalFormInt matrix =
  let rows = V.length matrix
      cols = V.length $ V.head matrix
      diag = min rows cols

      calc (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat
        else
          case chooseRowPivotInt (rowIndex, colIndex) rows cols mat of
            Just (True, mx)  ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColInt (rowIndex, colIndex) $ improveColInt rowIndex rows $
                  elimRowInt (rowIndex, colIndex) $ improveRowInt (rowIndex, colIndex) cols mx
            Just (False, mx) ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColInt (rowIndex, colIndex) $ improveColInt rowIndex rows mx
            Nothing          -> calc (rowIndex + 1, colIndex) mat

  in if V.null matrix then empty else finish diag $ calc (0, 0) matrix

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
elimColIntPar :: (Int, Int) -> IMatrix -> IMatrix
elimColIntPar (rowIndex, colIndex) elems =
  let pRow  = elems ! rowIndex
      pivot = pRow ! colIndex
      ri1   = rowIndex + 1
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
  in calc elems $ makeCoeffs ri1 $ V.drop ri1 $ V.map (\row -> row ! colIndex) elems

--gets the Smith normal form of a matrix, uses lots of parallelism if possible
normalFormIntPar :: IMatrix -> IMatrix
normalFormIntPar matrix =
  let rows = V.length matrix
      cols = V.length $ V.head matrix
      diag = min rows cols

      calc (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat
        else
          case chooseRowPivotInt (rowIndex, colIndex) rows cols mat of
            Just (True, mx)  ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColIntPar (rowIndex, colIndex) $ improveColIntPar rowIndex rows $
                  elimRowIntPar (rowIndex, colIndex) $ improveRowIntPar (rowIndex, colIndex) cols mx
            Just (False, mx) ->
              calc (rowIndex + 1, colIndex + 1) $
                elimColIntPar (rowIndex, colIndex) $ improveColIntPar rowIndex rows mx
            Nothing          -> calc (rowIndex + 1, colIndex) mat

  in if V.null matrix then empty else finish diag $ calc (0, 0) matrix

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
        | row ! i == 0 = elim (i + 1) ker img
        | otherwise               =
          let coeff      = (row ! i) `div` pivot
              transform1 = V.map (\r -> replaceElem i ((r ! i) - coeff*(r ! colIndex)) r)
              transform2 = \mat -> replaceElem colIndex ((coeff `mul` (mat ! i)) `add` (mat ! colIndex)) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
        where row = ker ! rowIndex
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

      result  = doColOps (0, 0) (toColEsch, toImage)
      colEsch = fst result
      image   = snd result
  in V.map (\i -> image ! i) $ V.filter (\i -> forallVec (\row -> row ! i == 0) colEsch) $ 0 `range` (cols - 1)

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
        | row ! i == 0 = elim (i + 1) ker img
        | otherwise               =
          let coeff      = (row ! i) `div` pivot
              transform1 = parMapVec (\r -> replaceElem i ((r ! i) - coeff*(r ! colIndex)) r)
              transform2 = \mat -> replaceElem colIndex ((coeff `mul` (mat ! i)) `add` (mat ! colIndex)) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
        where row = ker ! rowIndex
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
chooseGaussPivotBool (rowIndex, colIndex) mat = --assumes that i is a legal index for mat
  let row     = mat ! rowIndex --the following variable should be useful for quickly determining whether or not there are more entries to eleiminate
      indices = V.filter (\index -> index > colIndex) $ V.findIndices id row --but that method is not working for some reason
  in
    if not $ row ! colIndex then
      case indices of
        v | V.null v -> Nothing
        v            ->
          let j = V.head v
          in Just (not $ exactlyOneTrue row, V.map (switchElems colIndex j) mat, Just (colIndex, j))
    else Just (not $ exactlyOneTrue row, mat, Nothing)

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
          case chooseGaussPivotBool (rowIndex, colIndex) mat of
            Just (True, mx, _)  -> doColOps (rowIndex + 1, colIndex + 1) $ elimRowBool (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mat
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ V.map (\i ->
           if existsVec (\j -> mat ! j ! i /= 0) (0 `range` (rows - 1)) then 1 else 0) $ 0 `range` cols1
  in countNonZeroCols $ doColOps (0, 0) matrix

--NORMAL FORM-------------------------------------------------------------

--rearranges the matrix if necessary, returns the matrix paired with its pivot
--first argument is the index of the pivot row
chooseRowPivotBool :: Int -> Int -> Int -> BMatrix -> Maybe (Bool, BMatrix)
chooseRowPivotBool pIndex numRows numCols mat =
  let row      = mat ! pIndex
      rIndices = V.toList $ V.findIndices id row
  in
    if not $ row ! pIndex then
      case rIndices of
        (i:is)  -> Just ((L.length is) > 0, V.map (switchElems i pIndex) mat)
        []      ->
          case V.toList $ V.findIndices id $ V.map (\r -> r ! pIndex) mat of
            (i:is)  -> Just ((L.length is) > 0, switchElems i pIndex mat)
            []      ->
              case findElem (\(i, j) -> (mat ! i ! j)) $ L.concat $ L.map (\i -> L.zip (repeat i) [pIndex..numCols - 1]) [pIndex..numRows - 1] of
                Just (i, j) -> Just ((V.length $ V.findIndices id $ mat ! i) > 1, switchElems pIndex i $ V.map (switchElems pIndex j) mat)
                Nothing     -> Nothing
    else Just ((L.length rIndices) > 1, mat)

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
          case chooseRowPivotBool rowIndex rows cols mat of
              Just (False, new)  ->
                calc (rowIndex + 1, colIndex + 1) $ elimColBool (rowIndex, colIndex) rows new
              Just (True, new)   ->
                calc (rowIndex + 1, colIndex + 1) $ elimColBool (rowIndex, colIndex) rows $
                  elimRowBool (rowIndex, colIndex) cols new
              Nothing            -> mat
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
  in V.map (\i -> img ! i) $ V.filter (\i -> forallVec (\row -> not $ row ! i) ker) $ 0 `range` cols1

--IMAGE IN BASIS OF KERNEL------------------------------------------------

elimRowBoolWithInv :: (Int, Int) -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
elimRowBoolWithInv (rowIndex, colIndex) numCols kernel image =
  let row = kernel ! rowIndex
      elim i ker img
          | i == numCols  = (ker, img)
          | not $ row ! i = elim (i + 1) ker img
          | otherwise     =
            let transform1 = V.map (\r -> replaceElem i ((r ! i) + (r ! colIndex)) r)
                transform2 = \mat -> replaceElem colIndex ((mat ! i) `add` (mat ! colIndex)) mat
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
  in V.map (\i -> img ! i) $ V.filter (\i -> forallVec (\row -> not $ row ! i) ker) $ 0 `range` cols1

--INTEGER POLYNOMIALS-----------------------------------------------------

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

--MOD 2 POLYNOMIALS-------------------------------------------------------

data BMonomial = Zero | Power Int deriving (Eq, Show)
type BPolyMat  = Vector (Vector BMonomial)

a :: BMonomial -> BMonomial -> BMonomial
Zero `a` x         = x
x `a` Zero         = x
(Power n) `a` (Power m) = Power $ n + m

d :: BMonomial -> BMonomial -> BMonomial
Zero `d` _         = Zero
(Power n) `d` (Power m) =
  if m > n then error "Tried to divide monomial by a monomial of a higher degree, the result would not be a polynomial."
  else Power $ n - m

m :: BMonomial -> BMonomial -> BMonomial
(Power n) `m` (Power m) = Power $ n + m

instance Num BMonomial where
  (+) = a
  (*) = m
  negate = id
  signum = id
  abs    = id
  a - b  = a + (negate b)
  fromInteger = Power . fromIntegral

instance Ord BMonomial where
  Zero < (Power _)       = True
  (Power _) < Zero       = False
  (Power n) < (Power m)  = n < m
  Zero > (Power _)       = False
  (Power _) > Zero       = True
  (Power n) > (Power m)  = n > m
  Zero <= (Power _)      = True
  (Power _) <= Zero      = False
  (Power n) <= (Power m) = n <= m
  Zero >= (Power _)      = False
  (Power _) >= Zero      = True
  (Power n) >= (Power m) = n >= m

choosePolyPivotBool :: (Int, Int) -> BPolyMat -> Maybe (Bool, BPolyMat, Maybe (Int, Int))
choosePolyPivotBool (rowIndex, colIndex) mat =
  let row     = mat ! rowIndex
      elem    = row ! colIndex
      indices = V.filter (\index -> index >= colIndex) $ V.findIndices (\n -> n /= Zero) row
      len     = V.length indices
  in case len of
    0                           -> Nothing
    1 | indices ! 0 == colIndex -> Just (False, mat, Nothing)
    1                           -> Just (False, V.map (switchElems colIndex (indices ! 0)) mat, Just (colIndex, indices ! 0))
    _                           ->
      let index = foldRelation (\i j -> (row ! i) > (row ! j)) indices
      in Just (True, V.map (switchElems colIndex index) mat, Just (colIndex, index))

colOp :: Int -> Int -> BMonomial -> BPolyMat -> BPolyMat
colOp pIndex index coeff matrix =
  V.map (\row -> replaceElem index ((row ! index) + (coeff * (row ! pIndex))) row) matrix

elimRowPolyBool :: (Int, Int) -> Int -> BPolyMat -> BPolyMat
elimRowPolyBool (rowIndex, colIndex) numCols matrix =
  let row = matrix ! rowIndex
      elim i mat
        | i == numCols    = mat
        | Zero == row ! i = elim (i + 1) mat
        | otherwise       = elim (i + 1) $ colOp colIndex i ((row ! i) `d` (row ! colIndex)) mat
  in elim (colIndex + 1) matrix

eschelonFormBool :: BPolyMat -> BPolyMat
eschelonFormBool toColEsch =
  let rows  = V.length toColEsch
      cols  = V.length $ V.head toColEsch
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat
        else
          case choosePolyPivotBool (rowIndex, colIndex) mat of
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowPolyBool (rowIndex, colIndex) cols mat
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowPolyBool (rowIndex, colIndex) cols mx
            Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) mx
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) mat
            Nothing                       -> doColOps (rowIndex + 1, colIndex) mat

  in doColOps (0, 0) toColEsch

colOpWithInv :: Int -> Int -> BMonomial -> BPolyMat -> BPolyMat -> (BPolyMat, BPolyMat)
colOpWithInv pIndex index coeff matrix inverted =
  let transform1 = V.map (\row -> replaceElem index ((row ! index) + (coeff * (row ! pIndex))) row)
      transform2 = \mat -> replaceElem pIndex ((mat ! pIndex) `add` (coeff `mul` (mat ! index))) mat
  in (transform1 matrix, transform2 inverted)

elimRowPolyBoolWithInv :: (Int, Int) -> Int -> BPolyMat -> BPolyMat -> (BPolyMat, BPolyMat)
elimRowPolyBoolWithInv (rowIndex, colIndex) numCols matrix inverted =
  let row = matrix ! rowIndex
      elim :: Int -> (BPolyMat, BPolyMat) -> (BPolyMat, BPolyMat)
      elim i (ker, img)
        | i == numCols    = (ker, img)
        | Zero == row ! i = elim (i + 1) (ker, img)
        | otherwise       = elim (i + 1) $ colOpWithInv colIndex i ((row ! i) `d` (row ! colIndex)) ker img
  in elim (colIndex + 1) (matrix, inverted)

eschelonAndNextBool :: BPolyMat -> BPolyMat -> (BPolyMat, Vector Int, BPolyMat)
eschelonAndNextBool toColEsch toImage =
  let rows  = V.length toColEsch
      cols  = V.length $ V.head toColEsch
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) (ker, img) =
        if rowIndex == rows || colIndex == cols then (ker, img)
        else
          case choosePolyPivotBool (rowIndex, colIndex) ker of
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowPolyBoolWithInv (rowIndex, colIndex) cols ker img
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowPolyBoolWithInv (rowIndex, colIndex) cols mx $ switchElems i j img
            Just (False, mx, Just (i, j)) -> doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result  = doColOps (0, 0) (toColEsch, toImage)
      ker     = fst result
      img     = snd result
      indices = V.filter (\i -> forallVec (\row -> Zero == row ! i) ker) $ 0 `range` cols1
  in (ker, indices, V.map (\i -> img ! i) indices)