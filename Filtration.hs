{- |
Module     : Persistence.Filtration
Copyright  : (c) Eben Cowley, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains functions for constructing filtrations and computing persistent homology, persistence landscapes, and computing bottleneck distance between barcode diagrams.

A filtration is a finite sequence of simplicial complexes where each complex is a subset of the next. This means that a filtration can be thought of as a single simplicial complex where each of the simplices is labeled with a "filtration index" that represents the index in the sequence where that simplex enters the filtration.

One way to create a filtration, given a simplicial complex, a metric for the vertices, and a list of distances, is to loop through the distances from greatest to least: create a simplicial complex each iteration which excludes simplices that contain pairs of vertices which are further than the current distance apart. This method will produce a filtration of Vietoris-Rips complexes - each filtration index will correspond to a Rips complex whose scale is the corresponding distance.

NOTE: It's important that, even though the smallest filtration index represents the smallest scale at which the data is being anaylzed, all functions in this library receive your list of scales sorted in *decreasing* order.

An essential thing to note about the way this library is set up is the distinction between "fast" and "light" functions. Light functions call the metric every time distance between two points is required, which is a lot. Fast functions store the distances between points and access them in constant time, BUT this means they use O(n^2) memory with respect to the number of data points, so it's a really bad idea to use this optimization on substantially large data.

Persistent homology is the main event of topological data analysis. It allows one to identify clusters, tunnels, cavities, and higher dimensional holes that persist in the data throughout many scales. The output of the persistence algorithm is a barcode diagram. A single barcode represents the filtration index where a feature appears and the index where it disappears (if it does). Alternatively, a barcode can represent the scale at which a feature and the scale at which it ends. Thus, short barcodes are typically interpretted as sampling irregularities and long barcodes are interpretted as actual features of whatever the underlying data set represents.

After you've got the barcodes of a data set, you might want to compare it with that of a different data set. This is the purpose of bottleneck distance, which corresponds to the Hausdorff distance between barcode diagrams.

-}

module Filtration (
  -- * Types
    FilterSimplex
  , SimpleFiltration
  , Filtration
  , Extended (Finite, Infinity)
  , BarCode
  , Landscape
  -- * Utilities
  , sim2String
  , filtr2String
  , getComplex
  , getDimension
  , simple2Filtr
  -- * Construction
  , filterByWeightsFast
  , makeRipsFiltrationFast
  , filterByWeightsLight
  , makeRipsFiltrationLight
  -- * Persistent homology
  , indexBarCodes
  , indexBarCodesSimple
  , scaleBarCodes
  , scaleBarCodesSimple
  -- * Comparing barcode diagrams
  , indexMetric
  , bottleNeckDistance
  , bottleNeckDistances
  --, calcLandscape
  ) where

import Util
import Matrix
import SimplicialComplex

import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies
import Data.Algorithm.MaximalCliques

-- * Types

{- |
  This type synonym exists to make other synonyms more concise.
  Each simplex in a filtration is represented as a triple: its filtration index,
  the indices of its vertices in the original data, and the indices of its faces in the next lowest dimension.
  Edges do not have reference to their faces, as it would be redundant with their vertices.
  All simplices are sorted according to filtration index upon construction of the filtration. 
  In each dimension, all simplices are sorted in increasing order of filtration index, 
  and every simplices face indices are sorted in decreasing order; 
  both of these facts are critical to the computation of persistent homology.
-}
type FilterSimplex = (Int, Vector Int, Vector Int)

{- |
  A type representing a filtration whose vertices all have filtration index 0.
  Slightly faster and slightly less memory usage. The first component is simply the number of vertices.
  The second component is a vector with an entry for each dimension of simplices, starting at dimension 1 for edges.
-}
type SimpleFiltration = (Int, Vector (Vector FilterSimplex))

{- |
  Representation of a filtration which, unlike `SimpleFiltration`, can cope with vertices that have a non-zero
  filtration index. Vertices of the filtration are represented like all other simplices except that they don't their own have vertices or faces.
-}
type Filtration = Vector (Vector FilterSimplex)

-- | Type for representing inifinite bottleneck distance and infinite bar codes.
data Extended a = Finite a
                | Infinity
                | MinusInfty
                deriving Eq

-- | Convert the extended value to a string in the generic way.
instance Show a => Show (Extended a) where
  show (Finite a) = "Finite " L.++ (show a)
  show Infinity   = "infinity"

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

instance Num a => Num (Extended a) where

  _ + Infinity        = Infinity
  _ + MinusInfty      = MinusInfnty
  Infinity   + _      = Infinity
  MinusInfty + _      = MinusInfnty
  Finite x + Finite y = Finite (x + y)

  _ - Infinity        = MinusInfty
  _ - MinusInfty      = Infinity
  Infinity   - _      = Infinity
  MinusInfty - _      = MinusInfnty
  Finite x - Finite y = Finite (x - y)

  _ * Infinity        = Infinity
  _ * MinusInfty      = MinusInfnty
  Infinity   * _      = Infinity
  MinusInfty * _      = MinusInfnty
  Finite x * Finite y = Finite (x * y)

  abs Infinity    = Infinity
  abs MinustInfty = Infinity
  abs (Finite x)  = Finite $ abs x

-- | `(x, Finite y)` is a feature that appears at index/scale x and disappears at index/scale y, `(x, Infinity)` begins at x and doesn't disappear.
type BarCode a = (a, Extended a)

{- |
  A Persistence landscape is a certain type of piecewise linear function based on a barcode diagram.
  It can be represented simply as a list of critical points paired with critical values.
-}
type Landscape = Vector (Vector (Extended Double, Extended Double))

-- * Filtration utilities

-- | Shows all the information in a simplex.
sim2String :: FilterSimplex -> String
sim2String (index, vertices, faces) =
  "Filtration index: " L.++ (show index) L.++
    "; Vertex indices: " L.++ (show vertices) L.++
      "; Boundary indices: " L.++ (show faces) L.++ "\n"

-- | Shows all the information in a filtration.
filtr2String :: Either SimpleFiltration Filtration -> String
filtr2String (Left f)  =
  "Simple filtration:\n" L.++ ((intercalate "\n") $ toList
    $ V.map (L.concat . toList . (V.map sim2String)) $ snd f)
filtr2String (Right f) =
  (intercalate "\n") $ toList $ V.map (L.concat . toList . (V.map sim2String)) f

-- | Gets the simplicial complex specified by the filtration index. This is O(n) with respect to the number of simplices.
getComplex :: Int -> Either SimpleFiltration Filtration -> SimplicialComplex
getComplex index (Left (n, simplices)) =
  (n, V.map (V.map not1 . V.filter (\(i, _, _) -> i == index)) simplices)
getComplex index (Right simplices)     =
  (V.length $ V.filter (\v -> one v <= index) (V.head simplices),
    V.map (V.map not1 . V.filter (\(i, _, _) -> i == index)) (V.tail simplices))

-- | Return the dimension of the highest dimensional simplex in the filtration (constant time).
getDimension :: Either SimpleFiltration Filtration -> Int
getDimension (Left sf) = V.length $ snd sf
getDimension (Right f) = V.length f - 1

-- | Convert a simple filtration into an ordinary filtration.
simple2Filtr :: SimpleFiltration -> Filtration
simple2Filtr (n, x) =
  let x' = (V.map (\(i, v, _) -> (i, v, V.reverse v)) $ V.head x) `cons` (V.tail x)
  in (mapWithIndex (\i (a,b,c) -> (a,i `cons` V.empty,c)) $ V.replicate n (0, V.empty, V.empty)) `cons` x'

-- * Filtration construction

{- |
  Given a list of scales, a simplicial complex, and a weighted graph (see SimplicialComplex) which encodes a metric on the vertices,
  this function creates a filtration out of a simplicial complex by removing simplices that contain edges that are too long for each scale in the list.
  This is really a helper function to be called by makeRipsFiltrationFast, but I decided to expose it in case you have a simplicial complex and weighted graph lying around.
  The scales MUST be in decreasing order.
-}
filterByWeightsFast :: Ord a => [a] -> (SimplicialComplex, Graph a) -> SimpleFiltration
filterByWeightsFast scales ((numVerts, simplices), graph) =
  let edgeInSimplex edge simplex =
        (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      edgeTooLong scale edge     = scale <= (fst $ graph ! (edge ! 0) ! (edge ! 1))
      maxIndex                   = (L.length scales) - 1

      calcIndices 0 [] sc         = sc
      calcIndices i (scl:scls) sc =
        let longEdges = V.filter (edgeTooLong scl) $ V.map (\(i, v, f) -> v) $ V.head sc --find edges excluded by this scale
        in calcIndices (i - 1) scls $ V.map (V.map (\(j, v, f) ->
          if j == 0 then --if the simplex has not yet been assigned a fitration index
            if existsVec (\edge -> edgeInSimplex edge v) longEdges then (i, v, f) --if a long edge is in the simplex, assign it the current index
            else (0, v, f) --otherwise wait until next iteration
          else (j, v, f))) sc --otherwise leave it alone

      sortFiltration simplices =
        let sortedSimplices =
              V.map (quicksort (\((i, _, _), _) ((j, _, _), _) -> i > j)) $ --sorted in reverse order
                V.map (mapWithIndex (\i s -> (s, i))) simplices
            newFaces dim (i, v, f) =
              let findNew j =
                    case V.findIndex (\x -> snd x == j) $ sortedSimplices ! (dim - 1) of
                      Just k  -> k
                      Nothing -> error "Persistence.sortFiltration.newFaces.findNew"
              in (i, v, (V.map findNew f))
        in
          if V.null simplices then simplices
          else mapWithIndex (\i ss -> V.map ((newFaces i) . fst) ss) sortedSimplices

      sortBoundaries = V.map (V.map (\(i, v, f) -> (i, v, quicksort (\a b -> a <= b) f)))

  in (numVerts, sortBoundaries $ sortFiltration $ --sort the simplices by filtration index, then sort boundaries so that the boundary chains can be acquired easily
      calcIndices maxIndex (L.tail scales) $
        V.map (V.map (\(v, f) -> (0, v, f))) $ simplices)

{- |
  Given a list of scales, a metric, and a data set, this function constructs a filtration of the Vietoris-Rips complexes associated with the scales.
  The scales MUST be in decreasing order. Note that this a fast function, meaning it uses O(n^2) memory to quickly access distances where n is the number of data points.
-}
makeRipsFiltrationFast :: (Ord a, Eq b) => [a]
                       -> (b -> b -> a)
                       -> Either (Vector b) [b]
                       -> SimpleFiltration
makeRipsFiltrationFast scales metric dataSet =
  filterByWeightsFast scales $ makeRipsComplexFast (L.head scales) metric dataSet

-- | The same as filterbyWeightsFast except it uses far less memory at the cost of speed. Note that the scales must be in decreasing order.
filterByWeightsLight :: Ord a => [a] -> (b -> b -> a) -> Either (Vector b) [b] -> SimplicialComplex -> SimpleFiltration
filterByWeightsLight scales metric dataSet (numVerts, simplices) =
  let edgeInSimplex edge simplex =
        (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      vector                     = case dataSet of Left v -> v; Right l -> V.fromList l
      edgeTooLong scale edge     = scale <= (metric (vector ! (edge ! 0)) (vector ! (edge ! 1)))
      maxIndex                   = (L.length scales) - 1

      calcIndices 0 [] sc         = sc
      calcIndices i (scl:scls) sc =
        let longEdges = V.filter (edgeTooLong scl) $ V.map (\(i, v, f) -> v) $ V.head sc --find edges excluded by this scale
        in calcIndices (i - 1) scls $ V.map (V.map (\(j, v, f) ->
          if j == 0 then --if the simplex has not yet been assigned a fitration index
            if existsVec (\edge -> edgeInSimplex edge v) longEdges then (i, v, f) --if a long edge is in the simplex, assign it the current index
            else (0, v, f) --otherwise wait until next iteration
          else (j, v, f))) sc --otherwise leave it alone

      sortFiltration simplices =
        let sortedSimplices =
              V.map (quicksort (\((i, _, _), _) ((j, _, _), _) -> i > j)) $ --sorted in increasing order
                V.map (mapWithIndex (\i s -> (s, i))) simplices
            newFaces dim (i, v, f) =
              let findNew j =
                    case V.findIndex (\x -> snd x == j) $ sortedSimplices ! (dim - 1) of
                      Just k  -> k
                      Nothing -> error "Persistence.filterByWeightsLight.sortFiltration.newFaces.findNew"
              in (i, v, (V.map findNew f))
        in
          if V.null simplices then simplices
          else mapWithIndex (\i ss -> V.map ((newFaces i) . fst) ss) sortedSimplices

  in (numVerts, sortFiltration $ --sort the simplices by filtration index
      calcIndices maxIndex (L.tail scales) $
        V.map (V.map (\(v, f) -> (0, v, f))) $ simplices)

-- | Given a list of scales in decreasing order, a metric, and a data set, this constructs the filtration of Vietoris-Rips complexes corresponding to the scales.
makeRipsFiltrationLight :: (Ord a, Eq b) => [a]
                        -> (b -> b -> a)
                        -> Either (Vector b) [b]
                        -> SimpleFiltration
makeRipsFiltrationLight scales metric dataSet =
  filterByWeightsLight scales metric dataSet $ makeRipsComplexLight (L.head scales) metric dataSet

-- * Persistent Homology

type Chain = Vector Int --indices of the simplices in the sum

{- |
  The nth entry in the list will describe the n-dimensional topology of the filtration.
  That is, the first list will represent clusters, the second list will represent tunnels or punctures,
  the third will represent hollow volumes,
  and the nth index list will represent n-dimensional holes in the data.
  Features are encoded by the filtration indices where they appear and disappear.
-}
indexBarCodes :: Filtration -> Vector (Vector (BarCode Int))
indexBarCodes filtration =
  let maxdim = getDimension (Right filtration)

      --given a vector of indices of simplices which are marked and a vector of boundary chains paired with the indices of their simplices
      --remove the unmarked simplices from the chain
      removeUnmarked :: Vector Int -> Vector (Int, Chain) -> Vector (Int, Chain)
      removeUnmarked marked = V.map (\(i, c) -> (i, V.filter (\j -> V.elem j marked) c))

      --eliminate monomials in the boundary chain until it is no longer or there is a monomial which can't be eliminated
      removePivotRows :: Vector (Maybe Chain) -> Chain -> Chain
      removePivotRows slots chain =
        if V.null chain then V.empty
        else
          case slots ! (V.head chain) of
            Nothing -> chain
            Just c  -> removePivotRows slots (chain `uin` c)

      --given the indices of the marked simplices from the last iteration, slots from the last iteration, and boundary chains
      --mark the appropriate simplices, fill in the appropriate slots, and identify bar codes
      --boundary chains are paired with the index of their coresponding simplex
      makeFiniteBarCodes :: Int
                         -> Vector Int
                         -> Vector (Maybe Chain)
                         -> Vector (Int, Chain)
                         -> Vector (BarCode Int)
                         -> (Vector Int, Vector (Maybe Chain), Vector (BarCode Int))
      makeFiniteBarCodes dim newMarked slots boundaries barcodes =
        if V.null boundaries then (newMarked, slots, barcodes)
        else
          let boundary = V.head boundaries
              reduced  = removePivotRows slots $ snd boundary
          in
            --mark the simplex if its boundary chain is reduced to null
            if V.null reduced then
              makeFiniteBarCodes dim (newMarked `snoc` (fst boundary)) slots (V.tail boundaries) barcodes
            else
              let pivot = V.head reduced
              --put the pivot chain in the pivot's slot, add the new barcode to the list
              in makeFiniteBarCodes dim newMarked (replaceElem pivot
                (Just reduced) slots) (V.tail boundaries) ((one $ filtration ! (dim - 1) ! pivot,
                  Finite $ one $ filtration ! dim ! (fst boundary)) `cons` barcodes)

      --get the finite bar codes for each dimension
      loopFiniteBarCodes :: Int
                         -> Vector (Vector Int)
                         -> Vector (Vector (Maybe Chain))
                         -> Vector (Vector (BarCode Int))
                         -> (Vector (Vector Int), Vector (Vector (Maybe Chain)), Vector (Vector (BarCode Int)))
      loopFiniteBarCodes dim marked slots barcodes =
        if dim > maxdim then (marked, V.tail slots, (V.tail barcodes) V.++ (V.empty `cons` V.empty)) --the slots vector made when looping over the vertices will be null
        else
          let numSlots = if dim == 0 then 0 else V.length $ filtration ! (dim - 1) --see above
              boundaries =
                removeUnmarked (V.last marked) $ mapWithIndex (\i (_, _, f) -> (i, f)) $ filtration ! dim
              (newMarked, newSlots, newCodes) =
                makeFiniteBarCodes dim V.empty (V.replicate numSlots Nothing) boundaries V.empty
          in loopFiniteBarCodes (dim + 1) (marked `snoc` newMarked)
            (slots `snoc` newSlots) (barcodes V.++ (newCodes `cons` V.empty))

      --if a simplex isn't marked and has an empty slot, an infinite bar code begins at it's filtration index
      makeInfiniteBarCodes :: Int -> Vector Int -> Vector (Maybe Chain) -> Vector (BarCode Int)
      makeInfiniteBarCodes dim marked slots =
        V.map (\i -> (one $ filtration ! dim ! i, Infinity)) $ V.filter (\i -> slots ! i == Nothing) marked

      --add the infinite bar codes to the list of bar codes in each dimension
      loopInfiniteBarCodes :: Int
                           -> (Vector (Vector Int), Vector (Vector (Maybe Chain)), Vector (Vector (BarCode Int)))
                           -> Vector (Vector (BarCode Int))
      loopInfiniteBarCodes dim (marked, slots, barcodes) =
        if dim > maxdim then barcodes
        else
          loopInfiniteBarCodes (dim + 1) (marked, slots, replaceElem dim ((barcodes ! dim)
            V.++ (makeInfiniteBarCodes dim (marked ! dim) (slots ! dim))) barcodes)

  in V.map (V.filter (\(a, b) -> b /= Finite a))
    $ loopInfiniteBarCodes 0 $ loopFiniteBarCodes 0 V.empty V.empty V.empty

{--
{- |
  Same as indexBarCodes above except this function pairs every barcode with the indices of the vertices that
  start the barcode.
-}
indexBarCodeVertices :: Filtration -> [[(BarCode Int, Vector Int)]]
indexBarCodeVertices filtration =
  let maxdim = getDimension (Right filtration)

      --given a vector of indices of simplices which are marked and a vector of boundary chains paired with the indices of their simplices
      --remove the unmarked simplices from the chain
      removeUnmarked :: Vector Int -> Vector (Int, Chain) -> Vector (Int, Chain)
      removeUnmarked marked = V.map (\(i, c) -> (i, V.filter (\j -> V.elem j marked) c))

      --eliminate monomials in the boundary chain until it is no longer or there is a monomial which can't be eliminated
      --also keeps track of which simplex boundaries are added in the process of elimination
      removePivotRows :: Vector (Maybe (Chain, Int)) -> (Chain, Vector Int) -> (Chain, Vector Int)
      removePivotRows slots (chain, simplices) =
        if V.null chain then (V.empty, simplices)
        else
          case slots ! (V.head chain) of
            Nothing     -> (chain, simplices)
            Just (c, s) -> removePivotRows slots ((chain `uin` c), simplices `snoc` s)

      --given the indices of the marked simplices from the last iteration, slots from the last iteration, and boundary chains
      --mark the appropriate simplices, fill in the appropriate slots, and identify bar codes
      --boundary chains are paired with the index of their coresponding simplex
      makeFiniteBarCodes :: Int -> Int -> Vector (Int, Vector Int) -> Vector (Maybe (Chain, Int)) -> Vector (Int, Chain) -> [(BarCode Int, Vector Int)] -> (Vector (Int, Vector Int), Vector (Maybe (Chain, Int)), [(BarCode Int, Vector Int)])
      makeFiniteBarCodes dim index newMarked slots boundaries barcodes =
        if V.null boundaries then (newMarked, slots, barcodes)
        else
          let boundary             = V.head boundaries
              (reduced, simplices) = removePivotRows slots (snd boundary, V.empty)
              vertices             = V.foldl1 (V.++) $ V.map (\i -> two $ filtration ! dim ! i) simplices
          in
            --mark the simplex if its boundary chain is reduced to null
            if V.null reduced then makeFiniteBarCodes dim (index + 1) (newMarked `snoc` (fst boundary, vertices)) slots (V.tail boundaries) barcodes
            else
              let pivot = V.head reduced
              --put the pivot chain in the pivot's slot, add the new barcode to the list together with all the indices of the vertices of the simplices in the reduced chain
              in makeFiniteBarCodes dim (index + 1) newMarked (replaceElem pivot (Just (reduced, index)) slots) (V.tail boundaries)
                (((one $ filtration ! (dim - 1) ! pivot, Finite $ one $ filtration ! dim ! (fst boundary)), vertices):barcodes)

      --get the finite bar codes for each dimension
      loopFiniteBarCodes :: Int -> Vector (Vector (Int, Vector Int)) -> Vector (Vector (Maybe (Chain, Int))) -> [[(BarCode Int, Vector Int)]] -> (Vector (Vector (Int, Vector Int)), Vector (Vector (Maybe (Chain, Int))), [[(BarCode Int, Vector Int)]])
      loopFiniteBarCodes dim marked slots barcodes =
        if dim > maxdim then (marked, V.tail slots, (L.tail barcodes) L.++ [[]]) --the slots vector made when looping over the vertices will be null
        else
          let numSlots = if dim == 0 then 0 else V.length $ filtration ! (dim - 1) --see above
              boundaries = removeUnmarked (V.map fst $ V.last marked) $ mapWithIndex (\i (_, _, f) -> (i, f)) $ filtration ! dim
              (newMarked, newSlots, newCodes) = makeFiniteBarCodes 0 dim V.empty (V.replicate numSlots Nothing) boundaries []
          in loopFiniteBarCodes (dim + 1) (marked `snoc` newMarked) (slots `snoc` newSlots) (barcodes L.++ [newCodes])

      --if a simplex isn't marked and has an empty slot, an infinite bar code begins at it's filtration index
      makeInfiniteBarCodes :: Int -> Vector (Int, Vector Int) -> Vector (Maybe (Chain, Int)) -> [(BarCode Int, Vector Int)]
      makeInfiniteBarCodes dim marked slots =
        V.toList $ V.map (\(i, v) -> ((one $ filtration ! dim ! i, Infinity), v)) $ V.filter (\(i, _) -> slots ! i == Nothing) marked

      --add the infinite bar codes to the list of bar codes in each dimension
      loopInfiniteBarCodes :: Int -> (Vector (Vector (Int, Vector Int)), Vector (Vector (Maybe (Chain, Int))), [[(BarCode Int, Vector Int)]]) -> [[(BarCode Int, Vector Int)]]
      loopInfiniteBarCodes dim (marked, slots, barcodes) =
        if dim > maxdim then barcodes
        else
          loopInfiniteBarCodes (dim + 1) (marked, slots, replaceElemList dim ((barcodes !! dim)
            L.++ (makeInfiniteBarCodes dim (marked ! dim) (slots ! dim))) barcodes)

  in L.map (L.filter (\((a, b), _) -> b /= Finite a)) $ loopInfiniteBarCodes 0 $ loopFiniteBarCodes 0 V.empty V.empty []
--}

-- | Same as above except this function acts on filtrations whose vertices all have filtration index zero (for a very slight speedup).
indexBarCodesSimple :: SimpleFiltration -> Vector (Vector (BarCode Int))
indexBarCodesSimple (numVerts, allSimplices) =
  let removeUnmarked marked = V.filter (\x -> V.elem x marked)

      removePivotRows reduced chain =
        if V.null chain then chain
        else
          case reduced ! (V.head chain) of
            Nothing -> chain
            Just t  -> removePivotRows reduced (chain `uin` t) --eliminate the element corresponding to the pivot in a different chain

      makeBarCodesAndMark :: Int
                          -> Int
                          -> Vector Int
                          -> Vector (Maybe (Vector Int))
                          -> Vector (Int, Vector Int, Vector Int)
                          -> (Vector (BarCode Int), Vector Int)
                          -> (Vector (BarCode Int), Vector Int, Vector Int)
      makeBarCodesAndMark dim index marked reduced simplices (codes, newMarked)
        | V.null simplices = (codes, newMarked, V.findIndices (\x -> x == Nothing) reduced)
        | V.null d         =
          makeBarCodesAndMark dim (index + 1) marked reduced
            (V.tail simplices) (codes, newMarked `snoc` index)
        | otherwise        =
          let maxindex = V.head d
              begin    = one $ allSimplices ! (dim - 1) ! maxindex
          in makeBarCodesAndMark dim (index + 1) marked
            (replaceElem maxindex (Just d) reduced) (V.tail simplices)
              ((begin, Finite i) `cons` codes, newMarked)
        where (i, v, f) = V.head simplices
              d         = removePivotRows reduced $ removeUnmarked marked f

      makeEdgeCodes :: Int
                    -> Vector (Maybe (Vector Int))
                    -> Vector (Int, Vector Int, Vector Int)
                    -> (Vector (BarCode Int), Vector Int)
                    -> (Vector (BarCode Int), Vector Int, Vector Int)
      makeEdgeCodes index reduced edges (codes, marked)
        | V.null edges = (codes, marked, V.findIndices (\x -> x == Nothing) reduced)
        | V.null d     =
          makeEdgeCodes (index + 1) reduced (V.tail edges) (codes, marked `snoc` index)
        | otherwise    =
          makeEdgeCodes (index + 1) (replaceElem (V.head d)
            (Just d) reduced) (V.tail edges) ((0, Finite i) `cons` codes, marked)
        where (i, v, f) = V.head edges
              d         = removePivotRows reduced f

      makeFiniteBarCodes :: Int
                         -> Int
                         -> Vector (Vector (BarCode Int))
                         -> Vector (Vector Int)
                         -> Vector (Vector Int)
                         -> (Vector (Vector (BarCode Int)), Vector (Vector Int), Vector (Vector Int))
      makeFiniteBarCodes dim maxdim barcodes marked slots =
        if dim == maxdim then (barcodes, marked, slots)
        else
          let (newCodes, newMarked, unusedSlots) =
                makeBarCodesAndMark dim 0 (V.last marked) (V.replicate (V.length
                  $ allSimplices ! (dim - 1)) Nothing) (allSimplices ! dim) (V.empty, V.empty)
          in makeFiniteBarCodes (dim + 1) maxdim
            (barcodes V.++ (newCodes `cons` V.empty)) (marked `snoc` newMarked) (slots `snoc` unusedSlots)

      makeInfiniteBarCodes :: (Vector (Vector (BarCode Int)), Vector (Vector Int), Vector (Vector Int))
                           -> Vector (Vector (BarCode Int))
      makeInfiniteBarCodes (barcodes, marked, unusedSlots) =
        let 
            makeCodes :: Int -> Vector (BarCode Int) -> Vector (BarCode Int)
            makeCodes i codes =
              let slots = unusedSlots ! i; marks = marked ! i
              in codes V.++ (V.map (\j -> (one $ allSimplices ! (i - 1) ! j, Infinity)) $ slots |^| marks)
            loop :: Int -> Vector (Vector (BarCode Int)) -> Vector (Vector (BarCode Int))
            loop i v
              | V.null v  = V.empty
              | i == 0    =
                ((V.head v) V.++ (V.map (\j -> (0, Infinity))
                  $ (unusedSlots ! 0) |^| (marked ! 0))) `cons` (loop 1 $ V.tail v)
              | otherwise = (makeCodes i $ V.head v) `cons` (loop (i + 1) $ V.tail v)
        in loop 0 barcodes

      edges    = V.map (\(i, v, f) -> (i, v, (V.reverse v))) $ V.head allSimplices
      numEdges = V.length edges

      (fstCodes, fstMarked, fstSlots) =
        makeEdgeCodes 0 (V.replicate numVerts Nothing) edges (V.empty, V.empty)

      verts = 0 `range` (numVerts - 1)

  in
    V.map (V.filter (\(a, b) -> b /= Finite a))
      $ makeInfiniteBarCodes $ makeFiniteBarCodes 1
        (V.length allSimplices) (fstCodes `cons` V.empty)
          (verts `cons` (fstMarked `cons` V.empty)) (fstSlots `cons` V.empty)

{- |
  The nth entry in the list will again describe the n-dimensional topology of the filtration.
  However, features are encoded by the scales where they appear and disappear. For consistency,
  scales must be in decreasing order.
-}
scaleBarCodes :: Either (Vector a) [a] -> Filtration -> Vector (Vector (BarCode a))
scaleBarCodes scales filtration =
  let s = V.reverse $ (\a -> case a of Left v -> v; Right l -> V.fromList l) scales

      translateBarCode (i, Infinity) = (s ! i, Infinity)
      translateBarCode (i, Finite j) = (s ! i, Finite $ s ! j)

  in V.map (V.map translateBarCode) $ indexBarCodes filtration

{- |
  Same as above except acts only on filtrations whose vertices all have filtration index 0.
  Note that scales must be in decreasing order.
-}
scaleBarCodesSimple :: Either (Vector a) [a] -> SimpleFiltration -> Vector (Vector (BarCode a))
scaleBarCodesSimple scales filtration =
  let s = V.reverse $ (\a -> case a of Left v -> v; Right l -> V.fromList l) scales

      translateBarCode (i, Infinity) = (s ! i, Infinity)
      translateBarCode (i, Finite j) = (s ! i, Finite $ s ! j)

  in V.map (V.map translateBarCode) $ indexBarCodesSimple filtration

-- * Bottleneck distance

{- |
  The standard (Euclidean) metric between index barcodes.
  The distance between infinite and finite barcodes is infinite,
  and the distance between two infinite barcodes is the absolute value of the
  difference of their fst component.
-}
indexMetric :: BarCode Int -> BarCode Int -> Extended Double
indexMetric (_, Finite _) (_, Infinity) = Infinity
indexMetric (_, Infinity) (_, Finite _) = Infinity
indexMetric (i, Infinity) (j, Infinity) =
  Finite $ fromIntegral $ abs $ i - j
indexMetric (i, Finite j) (k, Finite l) =
  let x = i - k; y = j - l
  in Finite $ sqrt $ fromIntegral $ x*x + y*y

{- |
  Given a metric, return the Hausdorff distance
  (referred to as bottleneck distance in TDA) between the two sets.
  Returns noting if either list of barcodes is empty.
-}
bottleNeckDistance :: Ord b => (BarCode a -> BarCode a -> Extended b)
                   -> Vector (BarCode a)
                   -> Vector (BarCode a)
                   -> Maybe (Extended b)
bottleNeckDistance metric diagram1 diagram2
  | V.null diagram1 = Nothing
  | V.null diagram2 = Nothing
  | otherwise       =
    let first  = V.maximum $ V.map (\p -> V.minimum $ V.map (metric p) diagram2) diagram1
        second = V.maximum $ V.map (\p -> V.minimum $ V.map (metric p) diagram1) diagram2
    in Just $ max first second

-- |  Get's all the bottleneck distances; a good way to determine the similarity of the topology of two filtrations.
bottleNeckDistances :: Ord b => (BarCode a -> BarCode a -> Extended b)
                    -> Vector (Vector (BarCode a))
                    -> Vector (Vector (BarCode a))
                    -> Vector (Maybe (Extended b))
bottleNeckDistances metric diagrams1 diagrams2 =
  let d = (L.length diagrams1) - (L.length diagrams2)
  in
    if d >= 0 then (V.zipWith (bottleNeckDistance metric) diagrams1 diagrams2) V.++ (V.replicate d Nothing)
    else (V.zipWith (bottleNeckDistance metric) diagrams1 diagrams2) V.++ (V.replicate (-d) Nothing)

{--}
calcLandscape :: Vector (BarCode Int) -> Landscape
calcLandscape barcodes =
  let rel1 (i,j) (k,l) --should be double checked
        if i > k  then True
        else if i == k && j <= l then True
        else False

      greaterThan (i,j) (k,l) =
        case j of
          Infinity    -> True
          MinusInfnty -> False
          Finite j'   ->
            case l of
              Infinity    -> True
              MinusInfnty -> False
              Finite l'   ->
                if l' == j' then False
                else if l' < j' then False
                else True

      loop :: Int -> Landscape -> Vector (BarCode Double) -> Landscape
      loop index result barcodes =
        let 
            loop' :: Int -> Landscape -> BarCode Double -> Vector (BarCode Double) -> Landscape
            loop' i r (b, d) xs =
              case V.findIndex (greaterThan (b, d)) xs of
                Nothing ->
                  let r1 = V.head r; r2 = V.last r
                  in r1 `snoc` (r2 V.++ (V.fromList [(Finite d, 0.0), (0.0, Infinity)]))
                Just j  ->
                  let (b', d') = xs ! j
                      xs'      = (V.take j xs) V.++ (V.drop (j + 1) xs)
                      new      =
                        if b' >= d then
                          if b' == d then [(Finite 0.0, b')]
                          else [(Finite 0.0, d), (Finite 0.0, b')]
                        else []
                  in
                    case new of
                      [] ->
                        let new' = ((Finite 0.5)*(d + b'), (Finite 0.5)*(d - b'))
                            xs'' = orderedInsert new' xs'
                        in
                          if d' == Infinity then
                            let r1 = V.head r; r2 = V.last r
                            in r1 `snoc` (r2 V.++ (V.fromList [new', (Infinity, Infinity)]))
                          else
                            let new'' = (V.empty `snoc` new') `snoc` ((Finite 0.5)*(b' + d'), (Finite 0.5)*(d' - b'))
                            in loop' j (r1 `snoc` (r2 V.++ new'')) (b', d') xs''
                      _  ->
                        if d' == Infinity then
                          let r1 = V.head r; r2 = V.last r
                          in r1 `snoc` (r2 V.++ (V.fromList [new', (Infinity, Infinity)]))
                        else
                          let new'' = (V.empty `snoc` new') `snoc` ((Finite 0.5)*(b' + d'), (Finite 0.5)*(d' - b'))
                          in loop' j (r1 `snoc` (r2 V.++ new'')) (b', d') xs''

        in
          if V.null barcodes then result
          else
            let (b, d) = V.head barcodes; xs = V.tail barcodes in
            in
              if d == Infinity then
                if b == MinusInfty then
                  let result' = result
                        `snoc` (V.fromList [(MinusInfty, Infinity), (Infinity, Infinity)])
                          `snoc` (V.fromList [(MinusInfty, Finite 0.0), (b, Finite 0.0), (Infinity, Infinity)])
                  in
                    if b == Infinity then
                      let result'' = (V.init result') `snoc` ((V.last result) V.++ (MinusInfty, Infinity))
                      in loop' index result (b, d) xs
                let new = V.fromList
                        $ [ (MinusInfty, Finite 0.0)
                        , (Finite b, Finite 0.0)
                        , (Infinity, Infinity)
                        ]
                in
              else
                let new = V.fromList
                        $ [ (MinusInfty, Finite 0.0)
                        , (Finite b, Finite 0.0)
                        , (0.5*(d0 + b), 0.5*(d0 - b))
                        ]
                in
--}