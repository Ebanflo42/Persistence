{- |
Module     : Persistence.Util
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains miscellaneous utility functions used throughout the Persistence library.

-}

module Persistence.Util where

import GHC.Int

import Data.Bits
import Data.List           as L
import Data.ByteString     as B
import Data.Vector         as V
import Data.Vector.Unboxed as UV

import Control.Parallel
import Control.Parallel.Strategies
import Control.Applicative

-- * Numerics

-- | Multiply a vector by a scalar.
mul :: Unbox a => Num a => a -> UV.Vector a -> UV.Vector a
mul s = UV.map (*s)

{- |
  Add two vectors together component-wise.
  WARNING: If one vector is longer than the other,
  the longer vector will simply be cut off.
-}
add :: Unbox a => Num a => UV.Vector a -> UV.Vector a -> UV.Vector a
add = UV.zipWith (+)

{- |
  Subtract the second vector from the first vector component-wise.
  WARNING: If one vector is longer than the other,
  the longer vector will simply be cut off.
-}
subtr :: Unbox a => Num a => UV.Vector a -> UV.Vector a -> UV.Vector a
subtr = UV.zipWith (\x y -> x - y)

-- | Dot product. WARNING: If one vector is shorter than the other, it will simply be cut off.
dotProduct :: Unbox a => Num a => UV.Vector a -> UV.Vector a -> a
dotProduct u v = UV.sum $ UV.zipWith (*) u v

-- | Extended Euclidean algorithm. Finds the gcd of the two inputs plus the coefficients that multiply each input and sum to give the gcd.
extEucAlg :: Integral a => a -> a -> (a, a, a)
extEucAlg a b =
  let eeaHelper r s t =
        case snd r of
          0 -> (fst r, fst s, fst t)
          _ ->
            let r1    = fst r
                r2    = snd r
                s2    = snd s
                t2    = snd t
                q     = r1 `div` r2
                nextr = r1 - q*r2
                nexts = fst s - q*s2
                nextt = fst t - q*t2
            in eeaHelper (r2, nextr) (s2, nexts) (t2, nextt)
  in (\(x, y, z) -> if x < 0 then (-x, -y, -z) else (x, y, z)) $ eeaHelper (a, b) (0, 1) (1, 0)

-- | Returns whether or not the first number divides the second number. Returns false if the first number is 0.
divides :: Int -> Int -> Bool
0 `divides` b = False
a `divides` b
  | b < 0     = False
  | b == 0    = True
  | otherwise = a `divides` (b - (abs a))

-- | Assumes vectors are of the same length. Get's Manhattan distance between them
manhattanDist :: Unbox a => Num a => UV.Vector a -> UV.Vector a -> a
manhattanDist p q = UV.sum $ UV.zipWith (\x y -> abs $ x - y) p q

-- | Computes integer coordinates of a point in a grid whose spacing is given by the scale.
floorVec :: Unbox a => RealFrac a => Floating a => a -> UV.Vector a -> UV.Vector Int
floorVec scale = UV.map (floor . (/scale))

-- Euclidean metric
l2metric :: Unbox a => Floating a => UV.Vector a -> UV.Vector a -> a
l2metric p q = sqrt $ UV.sum $ UV.map (\x -> x*x) $ UV.zipWith (-) p q

-- * Boxed Vectors

-- | Strict folding left.
foldlS :: (a -> b -> a) -> a -> V.Vector b -> a
foldlS f init vec =
  let maxIndex      = V.length vec
      calc i result =
        result `pseq` if i == maxIndex then result else calc (i + 1) $ f result $ vec V.! i
  in calc 0 init

-- | Concatenate a vector of vectors.
flatten :: V.Vector (V.Vector a) -> V.Vector a
flatten v = foldlS (V.++) (V.head v) $ V.tail v

-- | Switches the elements of the vector at the given indices.
switchElems ::Int -> Int -> V.Vector a -> V.Vector a
switchElems i j vector
  | j == i    = vector
  | j < i     =
    let first  = V.take j vector
        second = V.drop (j + 1) (V.take i vector)
        third  = V.drop (i + 1) vector
    in first V.++ (V.cons (vector V.! i) second) V.++ (V.cons (vector V.! j) third)
  | otherwise =
    let first  = V.take i vector
        second = V.drop (i + 1) (V.take j vector)
        third  = V.drop (j + 1) vector
    in first V.++ (V.cons (vector V.! j) second) V.++ (V.cons (vector V.! i) third)

-- | Remove the element at the given index.
rmIndex :: Int -> V.Vector a -> V.Vector a
rmIndex i v = (V.take i v) V.++ (V.drop (i + 1) v)

-- | Generate a range of integers in vector form.
range :: Int -> Int -> V.Vector Int
range x y = V.drop x $ V.generate (y + 1) id

-- | Drop all elements from the right until the predicate is unsatisfied.
dropRightWhile :: (a -> Bool) -> V.Vector a -> V.Vector a
dropRightWhile p v =
  let calc i
        | i == 0            = 0
        | not $ p $ v V.! i = i
        | otherwise         = calc $ i - 1
  in V.take (calc ((V.length v) - 1) + 1) v

-- | Filter a vector with a predicate that takes into account the index of the element.
filterWithIndex :: (Int -> a -> Bool) -> V.Vector a -> V.Vector a
filterWithIndex p vector =
  let maxIndex = V.length vector - 1
  in V.map snd $ V.filter (\(i, x) -> p i x) $ V.zip (0 `range` maxIndex) vector

-- | Map a function that takes into account the index of each element.
mapWithIndex :: (Int -> a -> b) -> V.Vector a -> V.Vector b
mapWithIndex f vector =
  let maxIndex = V.length vector - 1
  in V.map (\(i, x) -> f i x) $ V.zip (0 `range` maxIndex) vector

-- | Parallel map a function over a vector.
parMapVec :: (a -> b) -> V.Vector a -> V.Vector b
parMapVec f = runEval . (evalTraversable rpar) . (V.map f)

-- | Map a function that takes into account the index of each element in parallel.
parMapWithIndex :: (Int -> a -> b) -> V.Vector a -> V.Vector b
parMapWithIndex f = runEval . (evalTraversable rpar) . (mapWithIndex f)

-- | Return the element satisfying the predicate and its index if it exists.
elemAndIndex :: (a -> Bool) -> V.Vector a -> Maybe (a, Int)
elemAndIndex p vector =
  let maxIndex = V.length vector
      helper i
        | i == maxIndex  = Nothing
        | p x            = Just (x, i)
        | otherwise      = helper (i + 1)
        where x = vector V.! i
  in helper 0

-- | Return the elements satisfying the predicate and their indices.
elemAndIndices :: (a -> Bool) -> V.Vector a -> [(a, Int)]
elemAndIndices p vector =
  let maxIndex = V.length vector
      helper i
        | i == maxIndex  = []
        | p x            = (x, i):(helper (i + 1))
        | otherwise      = helper (i + 1)
        where x = vector V.! i
  in helper 0

-- | Given a relation and two vectors, find all pairs of elements satisfying the relation.
findBothElems :: (a -> b -> Bool) -> V.Vector a -> V.Vector b -> V.Vector (a, b)
findBothElems rel vector1 vector2 =
  V.filter (\(a, b) -> rel a b) $ liftA2 (,) vector1 vector2

-- | Replace the element at the given index with the given element.
replaceElem :: Int -> a -> V.Vector a -> V.Vector a
replaceElem i e v = (V.take i v) V.++ (e `V.cons` (V.drop (i + 1) v))

-- | Quicksort treating the given predicate as the <= operator. Sorts in decreasing order.
quickSort :: (a -> a -> Bool) -> V.Vector a -> V.Vector a
quickSort rel vector = --rel is the <= operator
  if V.null vector then V.empty
  else
    let x       = V.head vector
        xs      = V.tail vector
        greater = V.filter (rel x) xs
        lesser  = V.filter (not . (rel x)) xs
    in (quickSort rel greater) V.++ (x `V.cons` (quickSort rel lesser))

-- | If the predicate is the <= operator and the vector is sorted in decreasing order, this inserts the value in the correct position.
orderedInsert :: (a -> a -> Bool) -> a -> V.Vector a -> V.Vector a
orderedInsert rel x vector = --rel is the <= operator
  case V.findIndex (\y -> y `rel` x) vector of
    Just i  ->
      case V.findIndex (\y -> x `rel` y) $ V.drop i vector of
        Just j  -> (V.take (i + j) vector) V.++ (x `V.cons` (V.drop (i + j) vector))
        Nothing -> (V.take i vector) V.++ (x `V.cons` (V.drop i vector))
    Nothing -> vector `V.snoc` x

-- | Takes the union of all of the vectors.
bigU :: Eq a => V.Vector (V.Vector a) -> V.Vector a
bigU vec =
  let exists x v
        | V.null v      = False
        | V.head v == x = True
        | otherwise     = exists x (V.tail v)
      union v1 v2 =
        if V.null v1 then v2
        else
          let x = V.head v1
          in
            if exists x v2 then union (V.tail v1) v2
            else union (V.tail v1) (x `V.cons` v2)
  in foldlS union (V.head vec) $ V.tail vec

-- | The element being searched for, the vector being searched, and the lower and upper bounds on the indices.
binSearch :: Ord a => a -> V.Vector a -> Int -> Int -> Maybe Int
binSearch value xs low high
  | high < low         = Nothing
  | xs V.! mid > value = binSearch value xs low (mid - 1)
  | xs V.! mid < value = binSearch value xs (mid + 1) high
  | otherwise          = Just mid
  where mid = low + ((high - low) `div` 2)

-- | Intersection of SORTED vectors.
(|^|) :: Ord a => V.Vector a -> V.Vector a -> V.Vector a
vector1 |^| vector2 =
  let len          = V.length vector2 - 1
      calc acc v =
        if V.null v then acc
        else
          let x = V.head v; xs = V.tail v
          in case binSearch x vector2 0 len of
            Just _  -> calc (x `V.cons` acc) xs
            Nothing -> calc acc xs
  in calc V.empty vector1

-- | If the relation were the "greater than" operator, this would find the minimum element of the vector.
foldRelation :: (a -> a -> Bool) -> V.Vector a -> a
foldRelation rel vec =
  let calc w v
        | V.null v  = w
        | rel w x   = calc x xs
        | otherwise = calc w xs
        where x = V.head v; xs = V.tail v
  in calc (V.head vec) (V.tail vec)

-- | Unsafe index finding.
elemIndexUnsafe :: Eq a => a -> V.Vector a -> Int
elemIndexUnsafe elem vector =
  let find i v
        | V.null v         = error "Persistence.Util.elemIndexUnsafe. This is a bug. Please email the Persistence maintainers."
        | V.head v == elem = i
        | otherwise        = find (i + 1) $ V.tail v
  in find 0 vector

-- | Union minus intersection of ordered vectors.
uin :: Ord a => V.Vector a -> V.Vector a -> V.Vector a
u `uin` v =
  let len = V.length v
      findAndInsert i elem vec
        | i == len  = vec `V.snoc` elem
        | elem == x = (V.take i vec) V.++ (V.drop i1 vec)
        | elem >  x = (V.take i vec) V.++ (elem `V.cons` (V.drop i vec))
        | otherwise = findAndInsert i1 elem vec
        where x = vec V.! i; i1 = i + 1
  in
    if V.null u then v
    else (V.tail u) `uin` (findAndInsert 0 (V.head u) v)

-- * Unboxed Vectors

-- | Concatenate a vector of unboxed vectors.
flattenUbxd :: Unbox a => V.Vector (UV.Vector a) -> UV.Vector a
flattenUbxd vec = foldlS (UV.++) (V.head vec) $ V.tail vec

-- | Remove the element at the given index.
rmIndexUbxd :: Unbox a => Int -> UV.Vector a -> UV.Vector a
rmIndexUbxd i v = (UV.take i v) UV.++ (UV.drop (i + 1) v)

-- | Generate a range of integers in unboxed vector form.
rangeUbxd :: Int -> Int -> UV.Vector Int
rangeUbxd x y = UV.drop x $ UV.generate (y + 1) id

-- | Return all vectors missing exactly one element from the original vector.
getCombos :: Unbox a => UV.Vector a -> V.Vector (UV.Vector a)
getCombos vector = V.map (\i -> rmIndexUbxd i vector) $ 0 `range` (UV.length vector - 1)

-- | Map a function that takes into account the index of each element.
mapWithIndexUbxd :: Unbox a => Unbox b => (Int -> a -> b) -> UV.Vector a -> UV.Vector b
mapWithIndexUbxd f vector =
  let len = UV.length vector
  in UV.map (\(i, x) -> f i x) $ UV.zip (0 `rangeUbxd` (len - 1)) vector

-- | Parallel map a function over an unboxed vector.
parMapUbxd :: Unbox a => Unbox b => (a -> b) -> UV.Vector a -> UV.Vector b
parMapUbxd f v =
  if UV.null v then UV.empty
  else
    let x = UV.head v; xs = UV.tail xs
    in runEval $ rseq (f x) >> rpar (parMapUbxd f xs)

-- | Orders a list of vectors from greatest to least length.
sortVecs :: Unbox a => [UV.Vector a] -> [UV.Vector a]
sortVecs []     = []
sortVecs (v:vs) =
  let len  = UV.length v
      less = sortVecs $ L.filter (\u -> UV.length u < len) vs
      more = sortVecs $ L.filter (\u -> UV.length u >= len) vs
  in more L.++ [v] L.++ less

replaceElemUbxd :: Unbox a => Int -> a -> UV.Vector a -> UV.Vector a
replaceElemUbxd i e v = (UV.take i v) UV.++ (e `UV.cons` (UV.drop (i + 1) v))

-- | The element being searched for, the vector being searched, and the lower and upper bounds on the indices.
binSearchUbxd :: Unbox a => Ord a => a -> UV.Vector a -> Int -> Int -> Maybe Int
binSearchUbxd value xs low high
  | high < low         = Nothing
  | xs UV.! mid > value = binSearchUbxd value xs low (mid - 1)
  | xs UV.! mid < value = binSearchUbxd value xs (mid + 1) high
  | otherwise          = Just mid
  where mid = low + ((high - low) `div` 2)

-- | snocs the element if an only if it isn't already in the vector.
smartSnoc :: Unbox a => Eq a => UV.Vector a -> a -> UV.Vector a
smartSnoc v e =
  case UV.elemIndex e v of
    Just _  -> v
    Nothing -> v `UV.snoc` e

-- * Lists

-- | Replace the element at the given index with the given element.
replaceElemList :: Int -> a -> [a] -> [a]
replaceElemList i e l = (L.take i l) L.++ (e:(L.drop (i + 1) l))

{- |
  Spark the first argument for parallel evaluation and force evaluation of the second argument,
  then return the first argument concatenated to the second. This is useful especially if the second
  argument is a recursive call that calls evalPar again, so that every elemment of the list will be
  sparked for parallelism.
-}
evalPar :: a -> [a] -> [a]
evalPar c r = runEval $ rpar c >> rseq r >> return (c:r)

-- * Tuples

-- | First element of a triple.
one (a, _, _) = a

-- | Second element of a triple.
two (_, b, _) = b

-- | Third element of a triple.
thr (_, _, c) = c

-- | Last two elements of a triple.
not1 (_, b, c) = (b, c)

-- | First and last elements of a triple.
not2 (a, _, c) = (a, c)

-- | First two elements of a triple.
not3 (a, b, _) = (a, b)

-- * Miscellaneous

-- | A few primes.
primes :: [Int]
primes = [2, 3, 5, 7, 9, 11, 13, 17, 19, 23, 29]

-- | Make an array containing the first n primes.
makeNPrimes :: Int -> UV.Vector Int
makeNPrimes n =
  if n <= L.length primes then UV.fromList $ L.take n primes
  else
    let need               = n - (L.length primes)
        calc :: Int -> [Int] -> Int -> [Int]
        calc 0 result _    = result
        calc left result x =
          if L.any (\p -> x `mod` p == 0) result
          then calc left result (x + 1)
          else calc (left - 1) (x:result) (x + 1)
        primList = calc need primes 30
    in UV.fromList primList

-- | Lowest power of 2 greater than the square root of the integer.
getArrLen :: Int -> Int
getArrLen num =
  let sqrtn = floor $ sqrt $ fromIntegral num
      pow2m = sqrtn .|. (sqrtn `shiftR` 1)
                    .|. (sqrtn `shiftR` 2)
                    .|. (sqrtn `shiftR` 4)
                    .|. (sqrtn `shiftR` 8)
                    .|. (sqrtn `shiftR` 16)
                    .|. (sqrtn `shiftR` 32)
  in pow2m + 1

-- | Test the nth bit of a bytestring. No index checks.
testBBit :: ByteString -> Int -> Bool
testBBit bytes n =
  let modulo8 = n .&. 7
  in testBit (bytes `B.index` (n `shiftR` 3)) modulo8

-- | Set the nth bit of a bytestring. No index checks.
setBBit :: ByteString -> Int -> ByteString
setBBit bytes n =
  let modulo8 = n .&. 7
      mult8   = n `shiftR` 3
      left = B.take mult8 bytes
      right = B.drop (mult8 + 1) bytes
      old = bytes `B.index` mult8
      new = setBit old modulo8
  in left `B.append` (B.cons new right)