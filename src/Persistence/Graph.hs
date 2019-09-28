{- |
Module     : Persistence.Utilities
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

A module for representing and constructing adjacency matrices of weighted graphs for various kinds of data.

-}

module Persistence.Graph (
  -- * Types
    Graph
  , Trajectory
  , Extended (Finite, MinusInfty, Infinity)
  -- * Utilities
  , indexGraph
  , fromInt
  , unExtend
  -- * Construction
  , encodeWeightedGraph
  , nbrhdGraph
  , nbrhdGraphPar
  , nbrhdGraphGrid
  , nbrhdGraphGridPar
  --, trajGraph
  --, trajGraphPar
) where

import Persistence.Util

import Data.Bits
import Data.IntSet         as I
import Data.List           as L
import Data.ByteString     as B
import Data.Vector         as V
import Data.Vector.Unboxed as UV

import Control.Parallel

-- * Types

{- |
  We use unboxed vectors to represent the lower triangle of the adjacency matrix of a weighted graph. The type `a` represents the weights and the boolean indicates whether or not there is a connection. Two unconnected vertices may have a weight since the weight may represent the distance between two data points which do not fall within a certain scale of each other.
  The array is one dimensional and we provide a custom indexing function.
-}
type Graph a = UV.Vector (a, Bool)

{- |
  A type for representing a trajectory of vectors whose coordinates are in the type `a`. `a` will typically be `Float` or `Double`.
-}
type Trajectory a = V.Vector (UV.Vector a)

{- |
  A type extending the number line to positive and negative infinity.
  Used for representing infinite barcodes, bottleneck distance, and persistence landscapes.
-}
data Extended a = Finite a
                | Infinity
                | MinusInfty
                deriving (Eq, Show)

{- |
  The ordering is inherited from the type a,
  Infinity is greater than everything else and MinusInfty is less than everything else.
-}
instance (Ord a, Eq a) => Ord (Extended a) where

  _ > Infinity        = False
  Infinity > _        = True
  Finite a > Finite b = a > b
  MinusInfty > _      = False
  _ > MinusInfty      = True

  Infinity >= _            = True
  _ >= Infinity            = False
  Finite a >= Finite b     = a >= b
  MinusInfty >= MinusInfty = True
  MinusInfty >= _          = False

  Infinity < _            = False
  _ < Infinity            = True
  Finite a < Finite b     = a < b
  MinusInfty < MinusInfty = False
  MinusInfty < _          = True

  _ <= Infinity        = True
  Infinity <= _        = False
  Finite a <= Finite b = a <= b
  MinusInfty <= _      = True
  _ <= MinusInfty      = False

-- | Arithmetic is defined in the canonical way based on the arithmetic of `a`.
instance Num a => Num (Extended a) where

  _ + Infinity        = Infinity
  _ + MinusInfty      = MinusInfty
  Infinity   + _      = Infinity
  MinusInfty + _      = MinusInfty
  Finite x + Finite y = Finite (x + y)

  _ - Infinity        = MinusInfty
  _ - MinusInfty      = Infinity
  Infinity   - _      = Infinity
  MinusInfty - _      = MinusInfty
  Finite x - Finite y = Finite (x - y)

  _ * Infinity        = Infinity
  _ * MinusInfty      = MinusInfty
  Infinity   * _      = Infinity
  MinusInfty * _      = MinusInfty
  Finite x * Finite y = Finite (x * y)

  abs Infinity    = Infinity
  abs MinusInfty  = Infinity
  abs (Finite x)  = Finite $ abs x

  fromInteger = Finite . fromInteger

  signum Infinity   = Finite (fromInteger 1)
  signum MinusInfty = Finite (fromInteger (-1))
  signum (Finite x) = Finite (signum x)

-- * Utilities

{- |
  The indexing function behaves as though the matrix were represented as a full square. That is, you only have to worry about the indices being greater than 0 and less than the total number of graph vertices.
-}
indexGraph :: Unbox a => Graph a -> (Int, Int) -> (a, Bool)
indexGraph graph (i, j) =
  if i < j then indexGraph graph (j, i)
  else
    let tri   = (i*(i + 1)) `shiftR` 1
        index = tri + j
    in graph UV.! index

-- | Convert extended integers to extended floats.
fromInt :: (Integral a, Floating b) => Extended a -> Extended b
fromInt (Finite i) = Finite $ fromIntegral i
fromInt Infinity   = Infinity
fromInt MinusInfty = MinusInfty

-- | Take `Infinity` to `1.0/0.0` and `MinusInfty` to `-1.0/0.0`.
unExtend :: Floating a => Extended a -> a
unExtend Infinity   = 1.0/0.0
unExtend (Finite x) = x
unExtend MinusInfty = -1.0/0.0

-- * Construction

{- |
  Given the number of vertices and a function which returns a weight and whether or not there is a connection, construct the graph.
  If your graph does not have weights, you can always use the () type.
-}
encodeWeightedGraph :: Unbox a => Int -> (Int -> Int -> (a, Bool)) -> Graph a
encodeWeightedGraph n f =
  let len    = (n*(n + 1)) `shiftR` 1
  in flattenUbxd $ V.map (\i ->
       UV.map (\j -> f i j) $ 0 `rangeUbxd` i) $ 0 `range` (n - 1)

-- | Parallel version of the above.
encodeWeightedGraphPar :: Unbox a => Int -> (Int -> Int -> (a, Bool)) -> Graph a
encodeWeightedGraphPar n f =
  let len    = (n*(n + 1)) `shiftR` 1
  in flattenUbxd $ parMapVec (\i ->
       UV.map (\j -> f i j) $ 0 `rangeUbxd` i) $ 0 `range` (n - 1)

-- | Given a scale, a metric, and some data, construct the neighborhood graph.
nbrhdGraph :: Unbox a => Ord a => a -> (b -> b -> a) -> Either (V.Vector b) [b] -> Graph a
nbrhdGraph scale metric dataSet =
  let vector = case dataSet of Right l -> V.fromList l; Left v -> v
      f i j  = let d = metric (vector V.! i) (vector V.! j) in (d, d < scale)
  in encodeWeightedGraph (V.length vector) f

-- | Paralell version of the above.
nbrhdGraphPar :: Unbox a => Ord a => a -> (b -> b -> a) -> Either (V.Vector b) [b] -> Graph a
nbrhdGraphPar scale metric dataSet =
  let vector = case dataSet of Right l -> V.fromList l; Left v -> v
      f i j  = let d = metric (vector V.! i) (vector V.! j) in (d, d < scale)
  in encodeWeightedGraphPar (V.length vector) f

{- |
  Use a grid and IntSets to construct the neighborhood graph of a point cloud in Euclidean space using the Euclidean metric. The data points are assumed to be unboxed 1-dimensional Repa arrays. For efficiency's sake, data points which are further away from each other than the scale are encoded as having distance `Infinity` between them; this does not affect the construction of the Rips filtration.
-}
nbrhdGraphGrid :: Unbox a
               => RealFrac a
               => Floating a
               => a
               -> Either (V.Vector (UV.Vector a)) [UV.Vector a]
               -> Graph a
nbrhdGraphGrid scale dataSet =
  let vector     = case dataSet of Right l -> V.fromList l; Left v -> v
      vlen       = V.length vector
      graphlen   = (vlen*(vlen + 1)) `shiftR` 1
      dimension  = UV.length $ V.head vector
      primArr    = makeNPrimes dimension
      arrlen     = getArrLen $ V.length vector
      getIndex p = (arrlen - 1) .&. (primArr `dotProduct` p)
      floors     = floorVec scale

      buildBuckets :: Int -> V.Vector IntSet -> V.Vector IntSet
      buildBuckets i result =
        if i == vlen then result
        else
          let index = getIndex $ floors $ vector V.! i
              buck  = result V.! index
          in buildBuckets (i + 1) $ replaceElem index (I.insert i buck) result

      buckets = buildBuckets 0 $ V.replicate vlen I.empty

      compute i =
        let p          = vector V.! i
            fp         = floors p
            neighbors  = L.map (add fp) $ UV.replicateM dimension [-1, 0, 1]
            allindices = unions $ L.map (\x -> buckets V.! (getIndex x)) neighbors
        in \j ->
          if j `member` allindices then
            let d = l2metric p (vector V.! j)
            in
              if d < scale then (d, True)
              else (1.0/0.0, False)
          else (1.0/0.0, False)

  in flattenUbxd $ mapWithIndex (\i r -> let f = compute i
       in f `pseq` UV.map f r) $ V.map (\i -> 0 `rangeUbxd` i) $ 0 `range` (vlen - 1)

-- | Parallel version of the above.
nbrhdGraphGridPar :: Unbox a
                  => RealFrac a
                  => Floating a
                  => a
                  -> Either (V.Vector (UV.Vector a)) [UV.Vector a]
                  -> Graph a
nbrhdGraphGridPar scale dataSet =
  let vector     = case dataSet of Right l -> V.fromList l; Left v -> v
      vlen       = V.length vector
      graphlen   = (vlen*(vlen + 1)) `shiftR` 1
      dimension  = UV.length $ V.head vector
      primArr    = makeNPrimes dimension
      arrlen     = getArrLen $ V.length vector
      getIndex p = (arrlen - 1) .&. (primArr `dotProduct` p)
      floors     = floorVec scale

      buildBuckets :: Int -> V.Vector IntSet -> V.Vector IntSet
      buildBuckets i result =
        if i == vlen then result
        else
          let index = getIndex $ floors $ vector V.! i
              buck  = result V.! index
          in buildBuckets (i + 1) $ replaceElem index (I.insert i buck) result

      buckets = buildBuckets 0 $ V.replicate vlen I.empty

      compute i =
        let p          = vector V.! i
            fp         = floors p
            neighbors  = L.map (add fp) $ UV.replicateM dimension [-1, 0, 1]
            allindices = unions $ L.map (\x -> buckets V.! (getIndex x)) neighbors
        in \j ->
          if j `member` allindices then
            let d = l2metric p (vector V.! j)
            in
              if d < scale then (d, True)
              else (1.0/0.0, False)
          else (1.0/0.0, False)

  in flattenUbxd
       $ parMapWithIndex (\i r -> let f = compute i
         in f `pseq` UV.map f r) $ V.map (\i -> 0 `rangeUbxd` i) $ 0 `range` (vlen - 1)