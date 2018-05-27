{- |
Module     : Persistence.Persistence
Copyright  : (c) Eben Cowley, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains functions for constructing filtrations and computing persistent homology, as well as a few utility functions for working with filtrations.

A filtration is a finite sequence of simplicial complexes where each complex is a subset of the next. This means that a filtration can be thought of as a single simplicial complex where each of the simplices is labeled with a "filtration index" that represents the index in the sequence where that simplex enters the filtration.

One way to create a filtration, given a simplicial complex, a metric for the vertices, and a list of distances, is to loop through the distances from greatest to least: create a simplicial complex each iteration which excludes simplices that contain pairs of vertices which are further than the current distance apart. This method will produce a filtration of Vietoris-Rips complexes - each filtration index will correspond to a VR complex whose scale is the corresponding distance.

An essential thing to note about the way this library is set up is the distinction between "fast" and "light" functions. Light functions call the metric every time distance between two points is required, which is a lot. Fast functions store the distances between points and access them in constant time, BUT this means they use O(n^2) memory with respect to the number of data points, so it's a really bad idea to use this optimization on substantially large data.

IMPORTANT NOTE: This library currently only handles filtrations where all the vertices have filtration index 0. Since I haven't thought of any way filtrations that don't respect this property could come up in applications, I have chosen to exclude them in favor of less memory usage and a slight speedup.

Persistent homology is the main event of topological data analysis. It allows one to identify clusters, holes, and voids that persist in the data throughout many scales. The output of the persistence algorithm is a barcode diagram, which currently have a somewhat crude representation in this library. A single barcode represents the filtration index where a feature appears and the index where it disappears (if it does). Thus, short barcodes are typically interpretted as noise and long barcodes are interpretted as actual features.

After you've got the persistent homology of a data set, you might want to compare it with that of a different data set. That's why this release includes two versions of "bottleneck distance," one works only if the number of features in each data set is the same and the other works regardless.

-}

module Persistence
  ( Filtration
  , BarCode
  , Extended
  , sim2String
  , filtr2String
  , getComplex
  , getNumSimplices
  , getDimension
  , filterByWeightsFast
  , makeVRFiltrationFast
  , filterByWeightsLight
  , makeVRFiltrationLight
  , persistentHomology
  ) where

import Util
import Matrix
import SimplicialComplex

import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies
import Data.Algorithm.MaximalCliques

--DATA TYPES--------------------------------------------------------------

{- |
  The first component is simply the number of vertices (all vertices are assumed to have filtration index 0).
  The second component is a vector with an entry for each dimension of simplices, starting at dimension 1 for edges.
  Each simplex is represented as a triple: its filtration index, the indices of its vertices in the original data, and the indices of its faces in the next lowest dimension.
  Edges do not have reference to their faces, as it would be redundant with their vertices. All simplices are sorted according to filtration index upon construction of the filtration.
-}
type Filtration = (Int, Vector (Vector (Int, Vector Int, Vector Int)))

-- | (i, Just j) is a feature that appears at filtration index i and disappears at index j, (i, Nothing) begins at i and doesn't disappear.
type BarCode = (Int, Maybe Int)

-- | Type for representing inifinite bottleneck distance.
data Extended a = Finite a | Infinity deriving Eq

instance (Ord a, Eq a) => Ord (Extended a) where
  Infinity > Infinity  = False
  Infinity > Finite _  = True
  Finite a > Finite b  = a > b
  Finite _ > Infinity  = False
  Infinity >= Infinity = True
  Infinity >= Finite _ = True
  Finite _ >= Infinity = False
  Finite a >= Finite b = a >= b
  Infinity < Infinity  = False
  Infinity < Finite a  = False
  Finite _ < Infinity  = True
  Finite a < Finite b  = a < b
  Infinity <= Infinity = True
  Infinity <= Finite _ = False
  Finite _ <= Infinity = True
  Finite a <= Finite b = a <= b

-- | Shows all the information in a simplex.
sim2String :: (Int, Vector Int, Vector Int) -> String
sim2String (index, vertices, faces) =
  "Filtration index: " L.++ (show index) L.++
    "; Vertex indices: " L.++ (show vertices) L.++
      "; Boundary indices: " L.++ (show faces) L.++ "\n"

-- | Shows all the information in a filtration.
filtr2String :: Filtration -> String
filtr2String = (intercalate "\n") . toList . (V.map (L.concat . toList . (V.map sim2String))) . snd

-- | Gets the simplicial complex specified by the filtration index. This is O(n) with respect to the number of simplices.
getComplex :: Int -> Filtration -> SimplicialComplex
getComplex index (n, simplices) = (n, V.map (V.map not1 . V.filter (\(i, _, _) -> i == index)) simplices)

{- |
  The first argument is a list of dimensions, the second argument is a list of filtration indices.
  The function returns the number of simplices in the filtration whose dimension and index exist in the respective lists.
-}
getNumSimplices :: [Int] -> [Int] -> Filtration -> Int
getNumSimplices dimensions indices (_, simplices) =
  L.length $ L.concat $
    L.map (\d -> V.toList $ V.filter (\(i, _, _) ->
      L.elemIndex i indices /= Nothing) $ simplices ! (d - 1)) dimensions

-- | Return the dimension of the highest dimensional simplex in the filtration (constant time).
getDimension :: Filtration -> Int
getDimension = V.length . snd

--FILTRATION CONSTRUCTION-------------------------------------------------

{- |
  Given a list of scales, a simplicial complex, and a weighted graph (see SimplicialComplex) which encodes a metric on the vertices,
  this function creates a filtration out of a simplicial complex by removing simplices that contain edges that are too long for each scale in the list.
  This is really a helper function to be called by makeVRFiltrationFast, but I decided to expose it in case you have a simplicial complex and weighted graph lying around.
  The scales MUST be in decreasing order for this function.
-}
filterByWeightsFast :: Ord a => [a] -> (SimplicialComplex, Graph a) -> Filtration
filterByWeightsFast scales ((numVerts, simplices), graph) =
  let edgeInSimplex edge simplex = (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
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
              V.map (quicksort (\((i, _, _), _) ((j, _, _), _) -> i > j)) $
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

  in (numVerts, sortFiltration $ --sort the simplices by filtration index
      calcIndices maxIndex (L.tail scales) $
        V.map (V.map (\(v, f) -> (0, v, f))) $ simplices)

{- |
  Given a list of scales, a metric, and a data set, this function constructs a filtration of the Vietoris-Rips complexes associated with the scales.
  The scales MUST be in decreasing order. Note that this a fast function, meaning it uses O(n^2) memory to quickly access distances where n is the number of data points.
-}
makeVRFiltrationFast :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeVRFiltrationFast scales metric dataSet = filterByWeightsFast scales $ makeVRComplexFast (L.head scales) metric dataSet

-- | The same as filterbyWeightsFast except it uses far less memory at the cost of speed. Note that the scales must be in decreasing order.
filterByWeightsLight :: Ord a => [a] -> (b -> b -> a) -> [b] -> SimplicialComplex -> Filtration
filterByWeightsLight scales metric dataSet (numVerts, simplices) =
  let edgeInSimplex edge simplex = (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      vector                     = V.fromList dataSet
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
              V.map (quicksort (\((i, _, _), _) ((j, _, _), _) -> i > j)) $
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
makeVRFiltrationLight :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeVRFiltrationLight scales metric dataSet = filterByWeightsLight scales metric dataSet $ makeVRComplexLight (L.head scales) metric dataSet

--PERSISTENT HOMOLOGY-----------------------------------------------------

{- |
  Each index in the list is a list of barcodes whose dimension corresponds to the index.
  That is, the first list will represent clusters, the second list will represent tunnels or punctures,
  the third will represent hollow volumes, and the nth index list will represent n-dimensional holes in the data.
-}
persistentHomology :: Filtration -> [[BarCode]]
persistentHomology (numVerts, allSimplices) =
  let
      --union minus intersection
      uin :: Ord a => Vector a -> Vector a -> Vector a
      u `uin` v =
        let findAndInsert elem vec =
              let (vec1, vec2) = biFilter (\x -> x > elem) vec
              in
                if V.null vec2 then vec `snoc` elem
                else if elem == V.head vec2 then vec1 V.++ V.tail vec2
                else vec1 V.++ (elem `cons` vec2)
        in
          if V.null u then v
          else (V.tail u) `uin` (findAndInsert (V.head u) v)
          
      removeUnmarked marked = V.filter (\x -> existsVec (\y -> y == x) marked)

      removePivotRows reduced chain =
        if V.null chain then chain
        else
          case reduced ! (V.head chain) of
            Nothing -> chain
            Just t  -> removePivotRows reduced (chain `uin` t) --eliminate the element corresponding to the pivot in a different chain

      makeBarCodesAndMark :: Int -> Int -> Vector Int -> Vector (Maybe (Vector Int)) -> Vector (Int, Vector Int, Vector Int) -> ([BarCode], Vector Int) -> ([BarCode], Vector Int, Vector Int)
      makeBarCodesAndMark dim index marked reduced simplices (codes, newMarked)
        | V.null simplices = (codes, newMarked, V.findIndices (\x -> x == Nothing) reduced)
        | V.null d         = makeBarCodesAndMark dim (index + 1) marked reduced (V.tail simplices) (codes, newMarked `snoc` index)
        | otherwise        =
          let maxindex = V.head d
              begin    = one $ allSimplices ! (dim - 1) ! maxindex
          in makeBarCodesAndMark dim (index + 1) marked (replaceElem maxindex (Just d) reduced) (V.tail simplices)
              ((begin, Just i):codes, newMarked)
        where (i, v, f) = V.head simplices
              d         = removePivotRows reduced $ removeUnmarked marked f

      makeEdgeCodes :: Int -> Vector (Maybe (Vector Int)) -> Vector (Int, Vector Int, Vector Int) -> ([BarCode], Vector Int) -> ([BarCode], Vector Int, Vector Int)
      makeEdgeCodes index reduced edges (codes, marked)
        | V.null edges = (codes, marked, V.findIndices (\x -> x == Nothing) reduced)
        | V.null d     =
          makeEdgeCodes (index + 1) reduced (V.tail edges) (codes, marked `snoc` index)
        | otherwise    =
          makeEdgeCodes (index + 1) (replaceElem (V.head d) (Just d) reduced) (V.tail edges) ((0, Just i):codes, marked)
        where (i, v, f) = V.head edges
              d         = removePivotRows reduced f

      makeFiniteBarCodes :: Int -> Int -> [[BarCode]] -> Vector (Vector Int) -> Vector (Vector Int) -> ([[BarCode]], Vector (Vector Int), Vector (Vector Int))
      makeFiniteBarCodes dim maxdim barcodes marked slots =
        if dim == maxdim then (barcodes, marked, slots)
        else
          let (newCodes, newMarked, unusedSlots) = makeBarCodesAndMark dim 0 (V.last marked) (V.replicate (V.length $ allSimplices ! (dim - 1)) Nothing) (allSimplices ! dim) ([], V.empty)
          in makeFiniteBarCodes (dim + 1) maxdim (barcodes L.++ [newCodes]) (marked `snoc` newMarked) (slots `snoc` unusedSlots)

      makeInfiniteBarCodes :: ([[BarCode]], Vector (Vector Int), Vector (Vector Int)) -> [[BarCode]]
      makeInfiniteBarCodes (barcodes, marked, unusedSlots) =
        let makeCodes i codes =
              let slots = unusedSlots ! i; marks = marked ! i
              in codes L.++ (V.toList $ V.map (\j -> (one $ allSimplices ! (i - 1) ! j, Nothing)) $ slots |^| marks)
            loop _ []     = []
            loop 0 (x:xs) = (x L.++ (V.toList $ V.map (\j -> (0, Nothing)) $ (unusedSlots ! 0) |^| (marked ! 0))):(loop 1 xs)
            loop i (x:xs) = (makeCodes i x):(loop (i + 1) xs)
        in loop 0 barcodes

      edges    = V.map (\(i, v, f) -> (i, v, (V.reverse v))) $ V.head allSimplices
      numEdges = V.length edges

      (fstCodes, fstMarked, fstSlots) = makeEdgeCodes 0 (V.replicate numVerts Nothing) edges ([], V.empty)

      verts = 0 `range` (numVerts - 1)

  in makeInfiniteBarCodes $ makeFiniteBarCodes 1 (V.length allSimplices) [fstCodes] (verts `cons` (fstMarked `cons` V.empty)) (fstSlots `cons` V.empty)

{- |
  Return the maximum of minimum distances bewteen the bar codes.
  It's important to note that the function isn't "unsafe" in the sense that it will throw an exception,
  it will just give you a distance regardless of whether or not there is the same number of barcodes is in each list.
-}
bottelNeckDistance :: [BarCode] -> [BarCode] -> Extended Double
bottelNeckDistance diagram1 diagram2 =
  let v1 = V.fromList diagram1
      v2 = V.fromList diagram2

      metric (x1, Just y1) (x2, Nothing) = Infinity
      metric (x1, Nothing) (x2, Just y1) = Infinity
      metric (x1, Just y1) (x2, Just y2) =
        let dx = fromIntegral $ x2 - x1; dy = fromIntegral $ y2 - y1
        in Finite $ sqrt $ dx*dx + dy*dy

  in foldRelation (<) $ V.map (\p -> foldRelation (>) $ V.map (metric p) v2) v1

-- |  Get's all the bottle neck distances; a good way to determine the similarity of the topology of two filtrations.
bottelNeckDistances :: [[BarCode]] -> [[BarCode]] -> [Extended Double]
bottelNeckDistances diagrams1 diagrams2 =
  let d = (L.length diagrams1) - (L.length diagrams2)
  in
    if d >= 0 then (L.zipWith bottelNeckDistance diagrams1 diagrams2) L.++ (L.replicate d Infinity)
    else (L.zipWith bottelNeckDistance diagrams1 diagrams2) L.++ (L.replicate (-d) Infinity)


-- | If the number of barcodes is the same, return the maximum of minimum distances bewteen the bar codes. Otherwise return nothing.
safeBottelNeckDistance :: [BarCode] -> [BarCode] -> Maybe (Extended Double)
safeBottelNeckDistance diagram1 diagram2 =
  if L.length diagram1 /= L.length diagram2 then Nothing
  else
    let v1 = V.fromList diagram1
        v2 = V.fromList diagram2

        metric (x1, Just y1) (x2, Nothing) = Infinity
        metric (x1, Nothing) (x2, Just y1) = Infinity
        metric (x1, Just y1) (x2, Just y2) =
          let dx = fromIntegral $ x2 - x1; dy = fromIntegral $ y2 - y1
          in Finite $ sqrt $ dx*dx + dy*dy

    in Just $ foldRelation (<) $ V.map (\p -> foldRelation (>) $ V.map (metric p) v2) v1

-- |  Safely get all the bottle neck distances; a good way to determine the similarity of the topology of two filtrations.
safeBottelNeckDistances :: [[BarCode]] -> [[BarCode]] -> [Maybe (Extended Double)]
safeBottelNeckDistances diagrams1 diagrams2 =
  let d = (L.length diagrams1) - (L.length diagrams2)
  in
    if d >= 0 then (L.zipWith safeBottelNeckDistance diagrams1 diagrams2) L.++ (L.replicate d Nothing)
    else (L.zipWith safeBottelNeckDistance diagrams1 diagrams2) L.++ (L.replicate (-d) Nothing)
