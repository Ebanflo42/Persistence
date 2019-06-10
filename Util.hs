{- |
Module     : Persistence.Util
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains miscellaneous utility functions used throughout the Persistence library.

-}

module Util where

import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

{- |
  Simple instance of Num where True is 1 and False is 0,
  all operations work like arithmetic modulo 2.
-}
instance Num Bool where
  p + q  = p `xor` q
  p * q  = p && q
  p - q  = p `xor` (not q)
  negate = not
  abs    = id
  fromInteger 0 = False
  fromInteger _ = True
  signum bool   = if bool then 1 else 0

-- | Exclusive or.
xor :: Bool -> Bool -> Bool
xor False False = False
xor True False  = True
xor False True  = True
xor True True   = False

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

-- | Concatenate a vector of vectors.
flatten :: Vector (Vector a) -> Vector a
flatten = V.foldl1 (V.++)

-- | Multiply a vector by a scalar.
mul :: Num a => a -> Vector a -> Vector a
mul s = V.map (*s)

{- |
  Add two vectors together component-wise.
  WARNING: If one vector is longer than the other,
  the longer vector will simply be cut off.
-}
add :: Num a => Vector a -> Vector a -> Vector a
add = V.zipWith (+)

{- |
  Subtract the second vector from the first vector component-wise.
  WARNING: If one vector is longer than the other,
  the longer vector will simply be cut off.
-}
subtr :: Num a => Vector a -> Vector a -> Vector a
subtr = V.zipWith (\x y -> x - y)

-- | Dot product. WARNING: vectors must be the same length.
dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct vec1 vec2
  | a && b = fromIntegral 0
  | a      = error "First vector passed to dotProduct too short."
  | b      = error "Second vector passed to dotProduct too short."
  | otherwise   = (V.head vec1)*(V.head vec2) + (dotProduct (V.tail vec1) (V.tail vec2))
    where a = V.null vec1; b = V.null vec2

{- |
  Extended Euclidean algorithm.
  Finds the gcd of the two inputs
  plus the coefficients that multiply each input and sum to give the gcd.
-}
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

{- |
  Returns whether or not the first number divides the second number.
  Instead of throwing an error, returns False if the first argument is 0.
-}
divides :: Int -> Int -> Bool
0 `divides` b = False
a `divides` b
  | b < 0     = False
  | b == 0    = True
  | otherwise = a `divides` (b - (abs a))

-- | Switches the elements of the vector at the given indices.
switchElems ::Int -> Int -> Vector a -> Vector a
switchElems i j vector
  | j == i    = vector
  | j < i     =
    let first  = V.take j vector
        second = V.drop (j + 1) (V.take i vector)
        third  = V.drop (i + 1) vector
    in first V.++ (cons (vector ! i) second) V.++ (cons (vector ! j) third)
  | otherwise =
    let first  = V.take i vector
        second = V.drop (i + 1) (V.take j vector)
        third  = V.drop (j + 1) vector
    in first V.++ (cons (vector ! j) second) V.++ (cons (vector ! i) third)

-- | Remove the element at the given index.
rmIndex :: Int -> Vector a -> Vector a
rmIndex i v = (V.take i v) V.++ (V.drop (i + 1) v)

-- | Generate a range of integers in vector form.
range :: Int -> Int -> Vector Int
range x y
  | x == y = x `cons` empty
  | x < y  = x `cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `snoc` y

{- |
  Return all vectors missing exactly one element from the original vector.
  Assumes the vector is non-empty.
-}
getCombos :: Vector a -> Vector (Vector a)
getCombos vector = V.map (\i -> rmIndex i vector) $ 0 `range` (V.length vector - 1)

-- | Map a function that takes into account the index of each element.
mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f vector =
  let helper i vec =
        if V.null vec then empty
        else cons (f i $ V.head vec) $ helper (i + 1) (V.tail vec)
  in helper 0 vector

-- | Filter a vector with a predicate that takes into account the index of the element.
filterWithIndex :: (Int -> a -> Bool) -> Vector a -> Vector a
filterWithIndex p vector =
  let maxIndex = V.length vector - 1
      calc i
        | i == maxIndex    = V.empty
        | p i (vector ! i) = (vector ! i) `cons` calc (i + 1)
        | otherwise        = calc (i + 1)
  in calc 0

-- | Parallel map a function over a vector.
parMapVec :: (a -> b) -> Vector a -> Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

-- | Map a function that takes into account the index of each element in parallel.
parMapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
parMapWithIndex f = runEval . (evalTraversable rpar) . mapWithIndex f

-- | Return the first element satisfying the predicate and its index if it exists.
elemAndIndex :: (a -> Bool) -> Vector a -> Maybe (a, Int)
elemAndIndex p vector =
  let helper i vec
        | V.null vec     = Nothing
        | p $ V.head vec = Just (V.head vec, i)
        | otherwise      = helper (i + 1) $ V.tail vec
  in helper 0 vector

-- | Return the elements satisfying the predicate and their indices.
elemAndIndices :: (a -> Bool) -> Vector a -> [(a, Int)]
elemAndIndices p vector =
  let helper i vec
        | V.null vec     = []
        | p $ V.head vec = (V.head vec, i) : (helper (i + 1) $ V.tail vec)
        | otherwise      = helper (i + 1) $ V.tail vec
  in helper 0 vector

-- | Given a relation and two vectors, find all pairs of elements satisfying the relation.
findBothElems :: (a -> b -> Bool) -> Vector a -> Vector b -> Vector (a, b)
findBothElems rel vector1 vector2 =
  flatten $ V.map (\a -> V.map (\b -> (a, b)) $ V.filter (rel a) vector2) vector1

-- | Orders a list of vectors from greatest to least length.
sortVecs :: [Vector a] -> [Vector a]
sortVecs =
  let ordering v1 v2
        | (V.length v1) > (V.length v2) = GT
        | (V.length v1) < (V.length v2) = LT
        | otherwise                     = EQ
  in L.sortBy ordering

-- | Replace the element at the given index with the given element.
replaceElem :: Int -> a -> Vector a -> Vector a
replaceElem i e v = (V.take i v) V.++ (e `cons` (V.drop (i + 1) v))

-- | Replace the element at the given index with the given element.
replaceElemList :: Int -> a -> [a] -> [a]
replaceElemList i e l = (L.take i l) L.++ (e:(L.drop (i + 1) l))

-- | Quicksort treating the given predicate as the <= operator. Sorts in decreasing order.
quickSort :: (a -> a -> Bool) -> Vector a -> Vector a
quickSort rel vector = --rel is the <= operator
  if V.null vector then empty
  else
    let x       = V.head vector
        xs      = V.tail vector
        greater = V.filter (rel x) xs
        lesser  = V.filter (not . (rel x)) xs
    in (quickSort rel greater) V.++ (x `cons` (quickSort rel lesser))

{- |
  If the predicate is the <= operator and the vector is sorted in decreasing order,
  this inserts the value in the correct position.
-}
orderedInsert :: (a -> a -> Bool) -> a -> Vector a -> Vector a
orderedInsert rel x vector =
  let (less, greater) = V.partition (rel x) vector
  in less V.++ (x `cons` greater)

-- | Takes the union of all of the vectors.
bigU :: Eq a => Vector (Vector a) -> Vector a
bigU =
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
            else union (V.tail v1) (x `cons` v2)
  in V.foldl1 union

{- |
  The element being searched for,
  the vector being searched,
  and the lower and upper bounds on the indices.
-}
binarySearch :: Ord a => a -> Vector a -> Int -> Int -> Maybe Int
binarySearch value xs low high
  | high < low        = Nothing
  | xs ! mid > value  = binarySearch value xs low (mid - 1)
  | xs ! mid < value  = binarySearch value xs (mid + 1) high
  | otherwise         = Just mid
  where mid = low + ((high - low) `div` 2)

-- | Intersection of SORTED vectors.
(|^|) :: Ord a => Vector a -> Vector a -> Vector a
vector1 |^| vector2 =
  let len          = V.length vector2 - 1
      calc acc v =
        if V.null v then acc
        else
          let x = V.head v; xs = V.tail v
          in case binarySearch x vector2 0 len of
            Just _  -> calc (x `cons` acc) xs
            Nothing -> calc acc xs
  in calc V.empty vector1

-- | snocs the element if an only if it isn't already in the vector.
smartSnoc :: Eq a => Vector a -> a -> Vector a
smartSnoc v e =
  case V.elemIndex e v of
    Just _  -> v
    Nothing -> v `snoc` e

{- |
  If the relation were the "greater than" operator,
  this would find the minimum element of the vector.
-}
foldRelation :: (a -> a -> Bool) -> Vector a -> a
foldRelation rel vec =
  let calc w v
        | V.null v  = w
        | rel w x   = calc x xs
        | otherwise = calc w xs
        where x = V.head v; xs = V.tail v
  in calc (V.head vec) (V.tail vec)

-- | Unsafe index finding.
elemIndexUnsafe :: Eq a => a -> Vector a -> Int
elemIndexUnsafe elem vector =
  let find i v
        | V.null v         = error "Element isn't here, Persistence.Util.elemIndexUnsafe"
        | V.head v == elem = i
        | otherwise        = find (i + 1) $ V.tail v
  in find 0 vector

{- |
  Spark the first argument for parallel evaluation and force evaluation of the second argument,
  then return the first argument concatenated to the second. This is useful especially if the second
  argument is a recursive call that calls evalPar again, so that every elemment of the list will be
  sparked for parallelism.
-}
evalPar :: a -> [a] -> [a]
evalPar c r = runEval $ rpar c >> rseq r >> return (c:r)

-- | Union minus intersection of ordered vectors.
uin :: Ord a => Vector a -> Vector a -> Vector a
u `uin` v =
  let len = V.length v
      findAndInsert i elem vec
        | i == len  = vec `snoc` elem
        | elem == x = (V.take i vec) V.++ (V.drop i1 vec)
        | elem >  x = (V.take i vec) V.++ (elem `cons` (V.drop i vec))
        | otherwise = findAndInsert i1 elem vec
        where x = vec ! i; i1 = i + 1
  in
    if V.null u then v
    else (V.tail u) `uin` (findAndInsert 0 (V.head u) v)