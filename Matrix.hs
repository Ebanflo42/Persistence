module Matrix where

import Util
import Data.List
import Control.Parallel

data Matrix a = Matrix [[a]] a Int Int

getElems (Matrix elems _ _ _) = elems --elements of the matrix
getOrder (Matrix _ order _ _) = order --modulus of the elements
getIndex (Matrix _ _ index _) = index --index of the current pivot
getMaxIndex (Matrix _ _ _ m)  = m

--given the *order* of the group and the elements of the matrix
--calculates the maximum pivot index and sets initial pivot index to zero
initializeMatrix :: Integral a => a -> [[a]] -> Matrix a
initializeMatrix order elems =
  Matrix elems order 0 ((min (length $ elems !! 0) (length elems)) - 1)

--increments the pivot index
incrementIndex :: Integral a => Matrix a -> Matrix a
incrementIndex (Matrix a b i c) = Matrix a b (i + 1) c

--shows the rows of an integer matrix separated by new lines
--as well as the modulus of the matrix
toString :: Matrix Int -> String 
toString matrix =
  let mat = flatten $ map (\row -> '\n':(flatten $ map (\e -> ' ':(show e)) row)) (getElems matrix) in
  mat ++ "\nmodulo " ++ (show $ getOrder matrix)

--first argument is a looping index, second argument is the upper bound for that index
--if a pivot row isn't found return nothing
--if it is found and the diagonal element is non-zero return the pivot index
--if it is found and is zero return the row index and the column index of the first non-zero element in the row
pivotHelper :: Integral a => Int -> Int -> [[a]] -> Maybe (Either Int (Int, Int))
pivotHelper i max [r]     =
  if i >= max then Nothing
  else if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else Just $ Left i
pivotHelper i max (r:rs) =
  if r !! i == 0 then
    case findIndex (\n -> n /= 0) r of
      Nothing -> pivotHelper (i + 1) max rs
      Just x  -> Just $ Right (i, x)
  else Just $ Left i

--calls pivot helper, rearranges the matrix if necessary, returns the matrix pair with its pivot
choosePivot :: Integral a => Matrix a -> (Maybe a, Matrix a)
choosePivot (Matrix elems ord i max) =
  case pivotHelper i max (drop i elems) of
    Nothing             -> (Nothing, Matrix elems ord i max)
    Just (Left x)       ->
      let row = elems !! x in
      if exactlyOneNonZero row then
        choosePivot $ Matrix elems ord (i + 1) max
      else
        (Just $ row !! x, Matrix elems ord x max)
    Just (Right (x, y)) ->
      let row = elems !! x in
      if exactlyOneNonZero row then
        choosePivot $ Matrix elems ord (i + 1) max
      else
        let pivot    = row !! y
            newElems = map (switchElems x y) elems in
      (Just pivot, Matrix newElems ord x max)

--given the index of the pivot, a pair, and a row of a matrix
--the first part of the pair is the gcd, bezout coefficients, and quotients
--second part of the pair is the index of the element to which that applies
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
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i) --may need to switch order
          newElems     = map (colOperationHelper pIndex transform) elems in
      improvePivot (newElems !! pIndex !! pIndex, Matrix newElems ord pIndex max)

--given a matrix whose pivot row has been improved, eliminates the entries in that row
eliminateEntries :: Integral a => Matrix a -> Matrix a
eliminateEntries (Matrix elems ord pIndex max) =
  let row      = elems !! pIndex
      pivot    = row !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  Matrix newElems ord pIndex max

--gets the smith normal form of a matrix
getSmithNormalForm :: Integral a => Matrix a -> Matrix a
getSmithNormalForm matrix =
  case choosePivot matrix of
    (Nothing, _)  -> matrix
    (Just p, mat) ->
      if p == 0 then error "Pivot was found to be zero."
      else
        let improved = improvePivot (p, mat)
            elim     = eliminateEntries improved
            pIndex   = getIndex elim
            ord      = getOrder elim
            elems    = getElems elim
            tr       = Matrix (transpose elems) ord pIndex (getMaxIndex elim) in
        case choosePivot tr of
          (Nothing, _)  -> (getSmithNormalForm . incrementIndex) tr
          (Just p, mat) -> (getSmithNormalForm . incrementIndex . eliminateEntries . improvePivot) (p, mat)