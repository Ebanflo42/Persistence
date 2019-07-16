{- |
Module     : Persistence.Matrix
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains a variety of matrix utility functions, used in the computation of Betti numbers and simplicial homology groups.

Most importantly, it includes functions for computing the rank, normal form, and kernel of matrices. For the computation of homology groups and Betti numbers, one must perform column operations on one matrix to get it into column echelon form and find its kernel while also performing the inverse row operations on the next matrix to be operated on.

Bool is an instance of Num here (instance given in Util) so that functions can be somewhat generalized to act on both integers and integers modulo 2.

-}

module Persistence.Matrix (
  -- * Types
    IMatrix
  , BMatrix
  -- * Utilities
  , getDiagonal
  , getUnsignedDiagonal
  , transposeMat
  , transposePar
  , multiply
  , multiplyPar
  -- Int matrices
  , rankInt
  , rankIntPar
  , normalFormInt
  , normalFormIntPar
  , kernelInt
  , kernelIntPar
  , imgInKerInt
  , imgInKerIntPar
  -- Bool matrices
  , rankBool
  , kernelBool
  , imgInKerBool
  ) where

{--FOR DEVS---------------------------------------------------------------

Matrices are transformed by iterating through each row and selecting a pivot. Zero rows are skipped for finding column eschelon form but a row operation is performed (if possible) if there is a zero row for Smith normal form.

To get the smith normal form, the entire pivot row and column is eliminated before continuing. Also, the pivot is always a diagonal element.

To get column eschelon form, every element in the pivot row after the pivot is eliminated. To get the kernel, all column operations to get the matrix to this form are also performed on the identiy matrix. To get the image of one matrix inside the kernel of the one being put into column eschelon form, perform the inverse row operations on the matrix whose image is needed. See stanford paper or the blog post on simplicial homology.

To get the rank of a matrix, look at the number of non-zero columns in the column eschelon form. To get the kernel, look at the columns of the identity (after all of the same column operations have been performed on it) which correspond to zero columns of the column eschelon form.

Eliminating elements is a slighltly more complicated process since only integer operations are allowed. First, every element that must be eliminated is made divisible by the pivot by using the Bezout coefficients from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used to eliminate the elements.

Boolean matrices are much easier to work with, they are regular matrices with elements modulo 2. Bool is an instance of Num here and the instance is given in Util.

--}

import Persistence.Util

import Data.List as L
import Data.Vector as V

import Control.Parallel.Strategies

-- * Types

-- | Matrix of integers.
type IMatrix = Vector (Vector Int)

-- | Matrix of integers modulo 2. Alternatively, matrix over the field with 2 elements.
type BMatrix = Vector (Vector Bool)

-- * Utilities

isMatrix :: Vector (Vector a) -> Bool
isMatrix mat =
  let rowLen = V.length $ V.head mat
  in V.all (\r -> V.length r == rowLen) mat

-- | Take the transpose a matrix (no fancy optimizations, yet).
transposeMat :: Vector (Vector a) -> Maybe (Vector (Vector a))
transposeMat mat =
  if isMatrix mat
  then Just $ V.map (\i -> V.map (\row -> row ! i) mat) $ 0 `range` ((V.length $ V.head mat) - 1)
  else Nothing

-- | Take the transpose of a matrix using parallel evaluation of rows.
transposePar :: Vector (Vector a) -> Vector (Vector a)
transposePar mat =
  parMapVec (\i -> V.map (\row -> row ! i) mat) $ 0 `range` ((V.length $ V.head mat) - 1)

-- | Multiply two matrices
multiply :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiply mat1 mat2 =
  let t =
        case transposeMat mat2 of
          Just m  -> m
          Nothing -> error "error in multiply"
  in V.map (\row -> V.map (dotProduct row) t) mat1

-- | Multiply matrices, evaluate rows in parallel if processors are available
multiplyPar :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
multiplyPar mat1 mat2 = runEval $ do
  let t =
        case transposeMat mat2 of
          Just m  -> m
          Nothing -> error "error in multiplyPar"
  rseq t
  return $ parMapVec (\row -> V.map (dotProduct row) t) mat1

-- | Get the diagonal elements.
getDiagonal :: Vector (Vector a) -> [a]
getDiagonal matrix =
  if V.null matrix then []
  else L.map (\i -> matrix ! i ! i) [0..(min (V.length matrix) (V.length $ V.head matrix)) - 1]

-- | Get the absolute value of each of the diagonal elements in a list.
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
        in first V.++ (cons (c11*elem1 + c12*elem2) second)
             V.++ (cons (c22*elem2 + c21*elem1) third)
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

-- * Int matrices

--RANK--------------------------------------------------------------------

--finds the pivot in a given row for Gaussian elimination given the index of the pivot row and the matrix
--returns whether or not the row needs to be eliminated with the rearranged matrix and
--the column switch performed (if there was one), returns Nothing if the row is all zeroes
chooseGaussPivotInt :: (Int, Int) -> IMatrix -> Maybe (Bool, IMatrix, Maybe (Int, Int))
chooseGaussPivotInt (rowIndex, colIndex) mat =
  let row     = mat ! rowIndex
      indices = V.filter (\index -> index > colIndex) $ V.findIndices (\x -> x /= 0) row
  in
    if row ! colIndex == 0 then
      if V.null indices then Nothing
      else
        let j = V.head indices
        in
          if row ! j == 0 then error "Persistence.Matrix.chooseGaussPivotInt. This is a bug. Please email the persistence maintainers."
          else Just (V.length indices > 1, V.map (switchElems colIndex j) mat, Just (colIndex, j))
    else Just (V.length indices > 0, mat, Nothing)

--does gaussian elimination on the pivot row of an integer matrix
improveRowInt :: (Int, Int) -> Int -> IMatrix -> IMatrix
improveRowInt (rowIndex, colIndex) numCols matrix =
  let improve i mat =
        if i == numCols then mat
        else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in
            --boundary operators have lots of zeroes
            --better to catch that instead of doing unnecessary %
            if pivot == 0 then
              if V.all (\a -> a == 0) row then mat
              else error "Persistence.Matrix.improveRowInt. This is a bug. Please email the Persistence maintainers."
            else
              if x == 0 || (x `mod` pivot == 0) then
                improve next mat
              else
                let gcdTriple = extEucAlg pivot x
                    gcd       = one gcdTriple
                in improve next $ colOperation colIndex i (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat

  in improve (colIndex + 1) matrix

--given pivot index and pivot paired with matrix whose pivot row has been improved
--eliminates the entries in the pivot row
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
        else
          let (i, coeff) = V.head ops
          in calc (mapWithIndex (\j row ->
               replaceElem i ((row ! i) - coeff*(pCol ! j)) row) mat) (V.tail ops)

  in
    if pivot == 0 then error "Persistence.Matrix.elimRowInt. This is a bug. Please email the Persistence maintainers."
    else calc elems $ makeCoeffs c1 $ V.drop c1 $ elems ! rowIndex

-- | Finds the rank of integer matrix (number of linearly independent columns).
rankInt :: IMatrix -> Int
rankInt matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat else
          case chooseGaussPivotInt (rowIndex, colIndex) mat of
            Just (True, mx, _)  ->
              doColOps (rowIndex + 1, colIndex + 1)
                $ elimRowInt (rowIndex, colIndex) $ improveRowInt (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mx
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ V.map (\i -> if V.any (\j -> mat ! j ! i /= 0)
          $ 0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

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
          in
            --boundary operators have lots of zeroes
            --better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next
                   $ colOperationPar colIndex i
                     (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
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
        else
          let (i, coeff) = V.head ops
          in
            calc (parMapWithIndex
              (\j row -> replaceElem i ((row ! i) - coeff*(pCol ! j)) row) mat) (V.tail ops)

  in
    if pivot == 0
    then error "Persistence.Matrix.elimRowIntPar. This is a bug. Please email the Persistence maintainers."
    else calc elems $ makeCoeffs c1 $ V.drop c1 $ elems ! rowIndex

-- | Calculates the rank of a matrix by operating on multiple rows in parallel.
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
        V.sum $ parMapVec (\i -> if V.any (\j -> mat ! j ! i /= 0) $
          0 `range` (rows - 1) then 1 else 0) $ 0 `range` cols1

  in countNonZeroCols $ doColOps (0, 0) matrix

--NORMAL FORM-------------------------------------------------------------

--rearranges matrix so that the pivot entry is in the correct position
--returns true if more elimination is necessary
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
          let pivot = matrix ! pIndex ! pIndex
              x     = matrix ! i ! pIndex
              next  = i + 1
          in
            --boundary operators have lots of zeroes
            --better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next
                   $ rowOperation pIndex i
                     (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
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
  in
    if pivot == 0
    then error "Persistence.Matrix.elimColInt. This is a bug. Please email the Persistence maintainters."
    else calc elems $ makeCoeffs ri1 $ V.drop ri1 $ V.map (\row -> row ! colIndex) elems

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
                improve   =
                  colOperation i i1
                    (thr gcdTriple, two gcdTriple, nextE `div` gcd, entry `div` gcd)
                cleanup   = \m -> elimColInt (i, i) $ elimRowInt (i, i) m
            in calc i1 $ cleanup $ improve mat'

      absDiag i mat =
        if i == V.length mat
        then mat
        else absDiag (i + 1)
               $ replaceElem i (replaceElem i (abs $ mat!i!i) $ mat!i) mat

      filtered = V.partition (\row -> V.any (\x -> x /= 0) row) matrix
  in absDiag 0 $ calc 0 $ (fst filtered) V.++ (snd filtered)

-- | Get the Smith normal form of an integer matrix.
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

  in
    if V.null matrix
    then empty
    else finish diag $ calc (0, 0) matrix

--improves the pivot column of a matrix in parallel
improveColIntPar :: Int -> Int -> IMatrix -> IMatrix
improveColIntPar pIndex maxIndex matrix =
  let improve i mat =
        if i == maxIndex then mat else
          let col   = V.map (\row -> row ! pIndex) mat
              pivot = col ! pIndex
              x     = col ! i
              next  = i + 1
          in
            --boundary operators have lots of zeroes
            --better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat
            else
              let gcdTriple = extEucAlg pivot x
                  gcd       = one gcdTriple
              in improve next
                   $ rowOperationPar pIndex i
                     (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd)) mat
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

-- | Gets the Smith normal form of a matrix, uses lots of parallelism if processors are available.
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

  in
    if V.null matrix
    then empty
    else finish diag $ calc (0, 0) matrix

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
                transform =
                  colOperation colIndex i
                    (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd))
            in improve next (transform mat) (transform ide)

  in improve (colIndex + 1) elems identity

--eliminates all the entries in the pivot row that come after the pivot
--after the matrix has been improved
--returns the new matrix (fst) and
--the identity with whatever column operations were performed (snd)
elimRowIntWithId :: (Int, Int) -> Int -> (IMatrix, Int, IMatrix) -> (IMatrix, IMatrix)
elimRowIntWithId (rowIndex, colIndex) numCols (elems, pivot, identity) =
  let row            = elems ! rowIndex
      elim i mat ide =
        if i == numCols then (mat, ide)
        else
          let coeff     = (row ! i) `div` pivot
              transform =
                V.map (\r -> (V.take i r)
                  V.++ (cons ((r ! i) - coeff*(r ! colIndex)) (V.drop (i + 1) r)))
          in elim (i + 1) (transform mat) (transform ide)
  in elim (colIndex + 1) elems identity

-- | Finds a basis for the kernel of a matrix, arranges the basis vectors into the rows of a matrix.
kernelInt :: IMatrix -> IMatrix
kernelInt matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i 0)
                   V.++ (cons 1 (V.replicate (cols1 - i) 0))) $ 0 `range` cols1

      doColOps (rowIndex, colIndex) (elems, ide) =
        if rowIndex == rows || colIndex == cols then (elems, ide) else
          case chooseGaussPivotInt (rowIndex, colIndex) elems of
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithId (rowIndex, colIndex) cols $
                improveRowIntWithId (rowIndex, colIndex) cols mx $ V.map (switchElems i j) ide
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowIntWithId (rowIndex, colIndex) cols $
                improveRowIntWithId (rowIndex, colIndex) cols elems ide
            Just (False, mx, Just (i, j)) ->
              doColOps (rowIndex + 1, colIndex + 1) (mx, V.map (switchElems i j) ide)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (elems, ide)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (elems, ide)

      result   = doColOps (0, 0) (matrix, identity)
      elems    = fst result
      ide      = snd result
  in V.map (\i -> V.map (\row -> row ! i) ide)
       $ V.filter (\i -> V.all (\row -> row ! i == 0) elems) $ 0 `range` cols1

--improves row in parallel and does the same thing to the identity matrix in parallel
improveRowIntWithIdPar :: (Int, Int) -> Int -> IMatrix -> IMatrix -> (IMatrix, Int, IMatrix)
improveRowIntWithIdPar (rowIndex, colIndex) numCols elems identity =
  let improve i mat ide =
        if i == numCols then (mat, mat ! rowIndex ! colIndex, ide) else
          let row   = mat ! rowIndex
              pivot = row ! colIndex
              x     = row ! i
              next  = i + 1
          in
            --boundary operators have lots of zeroes
            --better to catch that instead of doing unnecessary %
            if x == 0 || (x `mod` pivot == 0) then
              improve next mat ide
            else
            let gcdTriple = extEucAlg pivot x
                gcd       = one gcdTriple
                transform = colOperationPar colIndex i
                              (thr gcdTriple, two gcdTriple, x `div` gcd, -(pivot `div` gcd))
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
              transform = parMapVec (\r -> (V.take i r)
                            V.++ (cons ((r ! i) - coeff*(r ! colIndex)) (V.drop (i + 1) r)))
          in elim (i + 1) (transform mat) (transform ide)
  in elim (colIndex + 1) elems identity

-- | Computes basis vectors for the kernel of an integer matrix and arranges them into the rows of a matrix using lots of parallelism if processors are available.
kernelIntPar :: IMatrix -> IMatrix
kernelIntPar matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i 0)
                   V.++ (cons 1 (V.replicate (cols1 - i) 0))) $ 0 `range` cols1

      doColOps (rowIndex, colIndex) (elems, ide) =
        if rowIndex == rows || colIndex == cols then (elems, ide) else
          case chooseGaussPivotInt (rowIndex, colIndex) elems of
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1)
                $ elimRowIntWithIdPar (rowIndex, colIndex) cols
                  $ improveRowIntWithIdPar (rowIndex, colIndex) cols mx
                    $ V.map (switchElems i j) ide
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1)
                $ elimRowIntWithIdPar (rowIndex, colIndex) cols
                  $ improveRowIntWithIdPar (rowIndex, colIndex) cols elems ide
            Just (False, mx, Just (i, j)) ->
              doColOps (rowIndex + 1, colIndex + 1) (mx, V.map (switchElems i j) ide)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (elems, ide)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (elems, ide)

      result   = doColOps (0, 0) (matrix, identity)
      elems    = fst result
      ide      = snd result
  in V.map (\i -> V.map (\row -> row ! i) ide)
       $ V.filter (\i -> V.all (\row -> row ! i == 0) elems) $ 0 `range` cols1

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
                  transform1 =
                    colOperationPar colIndex i (thr gcdTriple, two gcdTriple, q2, -q1)
                  transform2 =
                    rowOperationPar colIndex i (-q1, -(two gcdTriple), -q2, thr gcdTriple)
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
              transform2 = \mat -> replaceElem colIndex
                             ((coeff `mul` (mat ! i)) `add` (mat ! colIndex)) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
        where row = ker ! rowIndex
  in elim (colIndex + 1) kernel image

-- | Calculates the image of the second matrix represented in the basis of the kernel of the first matrix.
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
          Just (False, mx, Just (i, j)) ->
            doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
          Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
          Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result  = doColOps (0, 0) (toColEsch, toImage)
      colEsch = fst result
      image   = snd result
  in V.map (\i -> image ! i)
       $ V.filter (\i -> V.all (\row -> row ! i == 0) colEsch) $ 0 `range` (cols - 1)

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
                  transform1 =
                    colOperationPar colIndex i (thr gcdTriple, two gcdTriple, q2, -q1)
                  transform2 =
                    rowOperationPar colIndex i (-q1, -(two gcdTriple), -q2, thr gcdTriple)
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
              transform2 = \mat -> replaceElem colIndex
                             ((coeff `mul` (mat ! i)) `add` (mat ! colIndex)) mat
          in elim (i + 1) (transform1 ker) (transform2 img)
        where row = ker ! rowIndex
  in elim (colIndex + 1) kernel image

-- | Calculates the image of the second matrix represented in the basis of the kernel of the first matrix. Uses lots of parallelism if processors are available.
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
          Just (False, mx, Just (i, j)) ->
            doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
          Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ker, img)
          Nothing                       -> doColOps (rowIndex + 1, colIndex) (ker, img)

      result = doColOps (0, 0) (toColEsch, toImage)
      ker    = fst result
      img    = snd result
  in V.map (\i -> img ! i)
       $ V.filter (\i -> V.all (\row -> row ! i == 0) ker) $ 0 `range` (cols - 1)

-- * Bool matrices

--RANK--------------------------------------------------------------------

--given the index of the pivot row and the matrix
--determines whether there is a non-zero element in the row, does necessary rearranging
--and returns the column operation that was performed if there was one
--returns Nothing if the entire row is zero
chooseGaussPivotBool :: (Int, Int) -> BMatrix -> Maybe (Bool, BMatrix, Maybe (Int, Int))
chooseGaussPivotBool (rowIndex, colIndex) mat =
  let row     = mat ! rowIndex
      indices = V.filter (\index -> index > colIndex) $ V.findIndices id row
  in
    if not $ row ! colIndex then
      if V.null indices then Nothing
      else
        let j = V.head indices
        in Just (V.length indices > 0, V.map (switchElems colIndex j) mat, Just (colIndex, j))
    else Just (V.length indices > 0, mat, Nothing)

--eliminates pivot row of a boolean matrix
elimRowBool :: (Int, Int) -> Int -> BMatrix -> BMatrix
elimRowBool (rowIndex, colIndex) numCols elems =
  let row = elems ! rowIndex
      elim i mat
        | i == numCols  = mat
        | not $ row ! i = elim (i + 1) mat
        | otherwise     = elim (i + 1)
                            $ V.map (\row -> replaceElem i ((row ! i) + (row ! colIndex)) row) mat
  in elim (colIndex + 1) elems

-- | Find the rank of a mod 2 matrix (number of linearly independent columns).
rankBool :: BMatrix -> Int
rankBool matrix =
  let rows  = V.length matrix
      cols  = V.length $ V.head matrix
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) mat =
        if rowIndex == rows || colIndex == cols then mat else
          case chooseGaussPivotBool (rowIndex, colIndex) mat of
            Just (True, mx, _)  ->
              doColOps (rowIndex + 1, colIndex + 1) $ elimRowBool (rowIndex, colIndex) cols mx
            Just (False, mx, _) -> doColOps (rowIndex + 1, colIndex + 1) mat
            Nothing             -> doColOps (rowIndex + 1, colIndex) mat

      countNonZeroCols mat =
        V.sum $ V.map (\i ->
           if V.any (\j -> mat ! j ! i /= 0) (0 `range` (rows - 1)) then 1 else 0) $ 0 `range` cols1
  in countNonZeroCols $ doColOps (0, 0) matrix

--KERNEL------------------------------------------------------------------

--eliminates all the entries in the pivot row that come after the pivot
--after the matrix has been improved
--returns the new matrix (fst) paired
--and the identity with whatever column operations were performed (snd)
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

-- | Finds the basis of the kernel of a matrix, arranges the basis vectors into the rows of a matrix.
kernelBool :: BMatrix -> BMatrix
kernelBool matrix =
  let rows     = V.length matrix
      cols     = V.length $ V.head matrix
      cols1    = cols - 1
      identity = V.map (\i -> (V.replicate i False)
                   V.++ (cons True (V.replicate (cols1 - i) False))) $ 0 `range` cols1

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
            Just (False, _, Just (i, j)) ->
              doColOps (rowIndex + 1, colIndex + 1) (ker, V.map (switchElems i j) ide)
            Just (False, _, _)           -> doColOps (rowIndex + 1, colIndex + 1) (ker, ide)
            Nothing                      -> doColOps (rowIndex + 1, colIndex) (ker, ide)

      result = doColOps (0, 0) (matrix, identity)
      ker    = fst result
      img    = snd result
  in V.map (\i -> img ! i) $ V.filter (\i -> V.all (\row -> not $ row ! i) ker) $ 0 `range` cols1

--IMAGE IN BASIS OF KERNEL------------------------------------------------

elimRowBoolWithInv :: (Int, Int) -> Int -> BMatrix -> BMatrix -> (BMatrix, BMatrix)
elimRowBoolWithInv (rowIndex, colIndex) numCols toColEch toImage =
  let row = toColEch ! rowIndex
      elim i ech img
          | i == numCols  = (ech, img)
          | not $ row ! i = elim (i + 1) ech img
          | otherwise     =
            let transform1 = V.map (\r -> replaceElem i ((r ! i) + (r ! colIndex)) r)
                transform2 = \mat -> replaceElem colIndex ((mat ! i) `add` (mat ! colIndex)) mat
            in elim (i + 1) (transform1 ech) (transform2 img)
  in elim (colIndex + 1) toColEch toImage

-- | Calculates the image of the second matrix represented in the basis of the kernel of the first matrix.
imgInKerBool :: BMatrix -> BMatrix -> BMatrix
imgInKerBool toColEch toImage =
  let rows  = V.length toColEch
      cols  = V.length $ V.head toColEch
      cols1 = cols - 1

      doColOps (rowIndex, colIndex) (ech, img) =
        if rowIndex == rows || colIndex == cols then (ech, img)
        else
          case chooseGaussPivotBool (rowIndex, colIndex) ech of
            Just (True, _, Nothing)       ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithInv (rowIndex, colIndex) cols ech img
            Just (True, mx, Just (i, j))  ->
              doColOps (rowIndex + 1, colIndex + 1) $
                elimRowBoolWithInv (rowIndex, colIndex) cols mx $ switchElems i j img
            Just (False, mx, Just (i, j)) ->
              doColOps (rowIndex + 1, colIndex + 1) (mx, switchElems i j img)
            Just (False, _, _)            -> doColOps (rowIndex + 1, colIndex + 1) (ech, img)
            Nothing                       -> doColOps (rowIndex + 1, colIndex) (ech, img)

      result = doColOps (0, 0) (toColEch, toImage)
      ker    = fst result
      img    = snd result
  in V.map (\i -> img ! i) $ V.filter (\i -> V.all (\row -> not $ row ! i) ker) $ 0 `range` cols1