{- |
Module     : Persistence.SimplicialComplex
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module provides functions for constructing neighborhood graphs, clique complexes, and Vietoris-Rips complexes, as well as the computation of Betti numbers.

A simplicial complex is like a generalization of a graph, where you can have any number of n-dimensional simplices (line-segments, triangles, tetrahedrons), glued along their boundaries so that the intersection of any two simplices is a simplicial complex. These objects are fundamental to topological data analysis.

An important thing to note about this module, and the library in general, is the difference between "fast" and "light" functions. Fast functions encode the metric in a 2D vector, so that distances don't need to be computed over and over again and can instead be accessed in constant time. Unfortunately, this takes O(n^2) memory so I would strongly recomend against using it for larger data sets unless you have a lot of RAM. "Light" functions do not use this speed optimization.

The neighborhood graph of point cloud data is a graph where an edge exists between two data points if and only if the vertices fall within a given distance of each other. Graphs here are more of a helper data type for constructing filtrations, which is why they have a somewhat odd representation. Not to worry, though, I've supplied a way of encoding graphs of a more generic representation.

The clique complex of a graph is a simplicial complex whose simplices are the complete subgraphs of the given graph. The Vietoris-Rips complex is the clique complex of the neighborhood graph.

Betti numbers measure the number of n-dimensional holes in a given simplicial complex. The 0th Betti number is the number of connected components, or clusters; the 1st Betti number is the number of loops or tunnels; the 2nd Betti number measures voids or hollow volumes; and if you have high-dimensional data, you might have higher Betti numbers representing the number of high-dimensional holes.

-}

module Persistence.SimplicialComplex (
  -- * Types
    Simplex
  , SimplicialComplex
  , Graph
  -- * Utilities
  , sc2String
  , getDim
  , encodeWeightedGraph
  , wGraph2sc
  , indexGraph
  -- * Construction
  , nbrhdGraph
  , nbrhdGraphPar
  , cliqueComplex
  , cliqueComplexPar
  , ripsComplexFast
  , ripsComplexFastPar
  , ripsComplexLight
  , ripsComplexLightPar
  -- * Homology
  --, bettiNumbers
  --, bettiNumbersPar
  --, simplicialHomology
  --, simplicialHomologyPar
  ) where

import Persistence.Util
--import Persistence.Matrix
import Persistence.Graph

import Data.Bits
import Data.List           as L
import Data.Vector         as V
import Data.Vector.Unboxed as UV
import Data.IntSet         as I

import Data.Algorithm.MaximalCliques

import Control.Parallel.Strategies

-- * Types

{- |
  A simplex is represented as a pair: the vector of its vertices (their index in the original data set), and the vector of the indices of the faces in the next lowest dimension of the simplicial complex.
  1-simplices are the exception: they do not store reference to their faces because it would be redundant with their vertices.
-}
type Simplex = (UV.Vector Int, UV.Vector Int)

{- |
  The first component of the pair is the number of vertices.
  The second component is a vector whose entries are vectors of simplices of the same dimension.
  Index 0 of the vecor corresponds to dimension 1 because there is no need to store individual vertices.
-}
type SimplicialComplex = (Int, V.Vector (V.Vector Simplex))

-- * Utilities

-- | Show all the information in a simplicial complex.
sc2String :: SimplicialComplex -> String
sc2String (v, allSimplices) =
  if V.null allSimplices then (show v) L.++ " vertices"
  else
  let edges     = V.head allSimplices
      simplices = V.tail allSimplices
      showSimplex s =
        '\n':(intercalate "\n" $ V.toList $ V.map show s)
      showAll sc    =
        if V.null sc then '\n':(show v) L.++ " vertices"
        else showSimplex (V.head sc) L.++ ('\n':(showAll (V.tail sc)))
  in (intercalate "\n" $ V.toList $ V.map (show . fst) edges) L.++ ('\n':(showAll simplices))

-- | Get the dimension of the highest dimensional simplex (V.constant time).
getDim :: SimplicialComplex -> Int
getDim = V.length . snd

-- | Given a weighted graph, V.construct a 1-dimensional simplicial complex in the canonical way. Betti numbers of this simplicial complex can be used to count cycles and connected components.
wGraph2sc :: Unbox a => Graph a -> SimplicialComplex
wGraph2sc graph =
  let numVerts              = round $ -0.5 + (sqrt $ 0.25 + 2.0*(fromIntegral $ UV.length graph))
      getEdges index result =
        if index == numVerts then result
        else
          let start = (index*(index + 1)) `shiftR` 1
              end   = start + index + 1
              new   = V.map (\(i, _) -> (UV.fromList [index, i], UV.empty)) $ V.filter snd
                        $ mapWithIndex (\i (_, b) -> (i, b)) $ UV.convert $ UV.slice start end graph
          in getEdges (index + 1) (result V.++ new)
  in (numVerts, (getEdges 0 V.empty) `V.cons` V.empty)

-- * Construction

{- |
  Makes a simplicial complex where the simplices are the complete subgraphs (cliques) of the given graph.
  Mainly a helper function for makeRipsComplexFast,
  but it might be useful if you happen to have a graph you want to analyze.
  I highly recomend using the parallel version, as this process is very expensive.
-}
cliqueComplex :: Unbox a => Graph a -> SimplicialComplex
cliqueComplex graph =
  let numVerts = round $ -0.5 + (sqrt $ 0.25 + 2.0*(fromIntegral $ UV.length graph))

      --make a list with an entry for every dimension of simplices
      organizeCliques :: Int -> [UV.Vector Int] -> [[UV.Vector Int]]
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        --find the simplices with the given number of vertices
        let helper = L.partition (\simplex -> i == UV.length simplex) cliques
        --append them to the next iteration of the function
        in (fst helper):(organizeCliques (i - 1) $ snd helper)

      --pair the organized maximal cliques with the dimension of the largest clique
      makePair :: [(UV.Vector Int)] -> (Int, [[UV.Vector Int]])
      makePair simplices =
        case simplices of
          (x:_) ->
            let dim = UV.length x
            in (dim, organizeCliques dim simplices)
          --if there are no maximal cliques this acts as a flag
          --so that the algorithm doesn't try to generate all the other simplices
          []    -> (1, [])

      --find all maximal cliques and sort them from largest to smallest
      --excludes maximal cliques which are single points
      maxCliques :: (Int, V.Vector (V.Vector (UV.Vector Int)))
      maxCliques =
        (\(x, y) -> (x, V.fromList $ L.map V.fromList y)) $ makePair
          $ sortVecs $ L.map UV.fromList $ L.filter (\c -> L.length c > 1)
            $ getMaximalCliques (\i j -> snd $ graph `indexGraph` (i, j)) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int
             -> Int
             -> V.Vector (V.Vector (UV.Vector Int))
             -> V.Vector (V.Vector Simplex)
             -> V.Vector (V.Vector Simplex)
      combos i max sc result =
        if i == max then
          --don't record boundary indices for edges
          (V.map (\s -> (s, UV.empty)) $ V.last sc) `V.cons` result
        else
          let i1        = i + 1
              --sc is in reverse order, so sc V.! i1 is the array of simplices one dimension lower
              current :: V.Vector (UV.Vector Int)
              current   = sc V.! i
              next :: V.Vector (UV.Vector Int)
              next      = sc V.! i1
              len       = V.length next
              allCombos :: V.Vector (V.Vector (UV.Vector Int))
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos :: V.Vector (UV.Vector Int)
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by
              --adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices :: V.Vector (UV.Vector Int)
              indices   = V.map (UV.convert . (V.map (\face ->
                            len + (elemIndexUnsafe face uCombos)))) allCombos
          in combos i1 max (replaceElem i1 (next
               V.++ uCombos) sc) $ (V.zip current indices) `V.cons` result

      fstmc2 = fst maxCliques - 2
  in
    --if there are no maximal cliques, the complex is just a bunch of points
    if fstmc2 == (-1) then (numVerts, V.empty)
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

-- | Parallel version of the above.
cliqueComplexPar :: Unbox a => Graph a -> SimplicialComplex
cliqueComplexPar graph =
  let numVerts = round $ -0.5 + (sqrt $ 0.25 + 2.0*(fromIntegral $ UV.length graph))

      --make a list with an entry for every dimension of simplices
      organizeCliques :: Int -> [UV.Vector Int] -> [[UV.Vector Int]]
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        --find the simplices with the given number of vertices
        let helper = L.partition (\simplex -> i == UV.length simplex) cliques
        --append them to the next iteration of the function
        in (fst helper):(organizeCliques (i - 1) $ snd helper)

      --pair the organized maximal cliques with the dimension of the largest clique
      makePair :: [(UV.Vector Int)] -> (Int, [[UV.Vector Int]])
      makePair simplices =
        case simplices of
          (x:_) ->
            let dim = UV.length x
            in (dim, organizeCliques dim simplices)
          --if there are no maximal cliques this acts as a flag
          --so that the algorithm doesn't try to generate all the other simplices
          []    -> (1, [])

      --find all maximal cliques and sort them from largest to smallest
      --excludes maximal cliques which are single points
      maxCliques :: (Int, V.Vector (V.Vector (UV.Vector Int)))
      maxCliques =
        (\(x, y) -> (x, V.fromList $ L.map V.fromList y)) $ makePair
          $ sortVecs $ L.map UV.fromList $ L.filter (\c -> L.length c > 1)
            $ getMaximalCliques (\i j -> snd $ graph `indexGraph` (i, j)) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int
             -> Int
             -> V.Vector (V.Vector (UV.Vector Int))
             -> V.Vector (V.Vector Simplex)
             -> V.Vector (V.Vector Simplex)
      combos i max sc result =
        if i == max then
          --don't record boundary indices for edges
          (V.map (\s -> (s, UV.empty)) $ V.last sc) `V.cons` result
        else
          let i1        = i + 1
              --sc is in reverse order, so sc V.! i1 is the array of simplices one dimension lower
              current :: V.Vector (UV.Vector Int)
              current   = sc V.! i
              next :: V.Vector (UV.Vector Int)
              next      = sc V.! i1
              len       = V.length next
              allCombos :: V.Vector (V.Vector (UV.Vector Int))
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos :: V.Vector (UV.Vector Int)
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by
              --adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices :: V.Vector (UV.Vector Int)
              indices   = parMapVec (UV.convert . (V.map (\face ->
                            len + (elemIndexUnsafe face uCombos)))) allCombos
          in combos i1 max (replaceElem i1 (next
               V.++ uCombos) sc) $ (V.zip current indices) `V.cons` result

      fstmc2 = fst maxCliques - 2
  in
    --if there are no maximal cliques, the complex is just a bunch of points
    if fstmc2 == (-1) then (numVerts, V.empty)
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

{- |
  Constructs the Vietoris-Rips complex given a scale, metric, and data set.
  Also uses O(n^2) memory (where n is the number of data points)
  for a graph storing all the distances between data points.
-}
ripsComplexFast :: Ord a
                => Unbox a
                => Eq b
                => a
                -> (b -> b -> a)
                -> Either (V.Vector b) [b]
                -> (SimplicialComplex, Graph a)
ripsComplexFast scale metric dataSet =
  let graph = nbrhdGraph scale metric dataSet
      sc    = cliqueComplex graph
  in (sc, graph)

-- | Parallel version of the above.
ripsComplexFastPar :: Ord a
                   => Unbox a
                   => Eq b
                   => a
                   -> (b -> b -> a)
                   -> Either (V.Vector b) [b]
                   -> (SimplicialComplex, Graph a)
ripsComplexFastPar scale metric dataSet =
  let graph = nbrhdGraphPar scale metric dataSet
      sc    = cliqueComplexPar graph
  in (sc, graph)

-- | Constructs the Vietoris-Rips complex given a scale, metric, and data set.
ripsComplexLight :: (Ord a, Eq b)
                 => a
                 -> (b -> b -> a)
                 -> Either (V.Vector b) [b]
                 -> SimplicialComplex
ripsComplexLight scale metric dataSet =
  let vector   = case dataSet of Right l -> V.fromList l; Left v -> v
      numVerts = V.length vector

      --make a list with an entry for every dimension of simplices
      organizeCliques :: Int -> [UV.Vector Int] -> [[UV.Vector Int]]
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        --find the simplices with the given number of vertices
        let helper = L.partition (\simplex -> i == UV.length simplex) cliques
        --append them to the next iteration of the function
        in (fst helper):(organizeCliques (i - 1) $ snd helper)

      --pair the organized maximal cliques with the dimension of the largest clique
      makePair :: [(UV.Vector Int)] -> (Int, [[UV.Vector Int]])
      makePair simplices =
        case simplices of
          (x:_) ->
            let dim = UV.length x
            in (dim, organizeCliques dim simplices)
          --if there are no maximal cliques this acts as a flag
          --so that the algorithm doesn't try to generate all the other simplices
          []    -> (1, [])

      --find all maximal cliques and sort them from largest to smallest
      --excludes maximal cliques which are single points
      maxCliques :: (Int, V.Vector (V.Vector (UV.Vector Int)))
      maxCliques =
        (\(x, y) -> (x, V.fromList
          $ L.map V.fromList y)) $ makePair
            $ sortVecs $ L.map UV.fromList $ L.filter
              (\c -> L.length c > 1) $ getMaximalCliques
                (\i j -> metric (vector V.! i) (vector V.! j) < scale) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int
             -> Int
             -> V.Vector (V.Vector (UV.Vector Int))
             -> V.Vector (V.Vector Simplex)
             -> V.Vector (V.Vector Simplex)
      combos i max sc result =
        if i == max then
          --don't record boundary indices for edges
          (V.map (\s -> (s, UV.empty)) $ V.last sc) `V.cons` result
        else
          let i1        = i + 1
              --sc is in reverse order, so sc V.! i1 is the array of simplices one dimension lower
              current :: V.Vector (UV.Vector Int)
              current   = sc V.! i
              next :: V.Vector (UV.Vector Int)
              next      = sc V.! i1
              len       = V.length next
              allCombos :: V.Vector (V.Vector (UV.Vector Int))
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos :: V.Vector (UV.Vector Int)
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by
              --adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices :: V.Vector (UV.Vector Int)
              indices   = V.map (UV.convert . (V.map (\face ->
                            len + (elemIndexUnsafe face uCombos)))) allCombos
          in combos i1 max (replaceElem i1 (next
               V.++ uCombos) sc) $ (V.zip current indices) `V.cons` result

      fstmc2 = fst maxCliques - 2
  in
    --if there are no maximal cliques, the complex is just a bunch of points
    if fstmc2 == (-1) then (numVerts, V.empty)
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

-- | Parallel version of the above.
ripsComplexLightPar :: Ord a
                    => Unbox a
                    => Eq b
                    => a
                    -> (b -> b -> a)
                    -> Either (V.Vector b) [b]
                    -> SimplicialComplex
ripsComplexLightPar scale metric dataSet =
  let vector   = case dataSet of Right l -> V.fromList l; Left v -> v
      numVerts = V.length vector

      --make a list with an entry for every dimension of simplices
      organizeCliques :: Int -> [UV.Vector Int] -> [[UV.Vector Int]]
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        --find the simplices with the given number of vertices
        let helper = L.partition (\simplex -> i == UV.length simplex) cliques
        --append them to the next iteration of the function
        in (fst helper):(organizeCliques (i - 1) $ snd helper)

      --pair the organized maximal cliques with the dimension of the largest clique
      makePair :: [(UV.Vector Int)] -> (Int, [[UV.Vector Int]])
      makePair simplices =
        case simplices of
          (x:_) ->
            let dim = UV.length x
            in (dim, organizeCliques dim simplices)
          --if there are no maximal cliques this acts as a flag
          --so that the algorithm doesn't try to generate all the other simplices
          []    -> (1, [])

      --find all maximal cliques and sort them from largest to smallest
      --excludes maximal cliques which are single points
      maxCliques :: (Int, V.Vector (V.Vector (UV.Vector Int)))
      maxCliques =
        (\(x, y) -> (x, V.fromList
          $ L.map V.fromList y)) $ makePair
            $ sortVecs $ L.map UV.fromList $ L.filter
              (\c -> L.length c > 1) $ getMaximalCliques
                (\i j -> metric (vector V.! i) (vector V.! j) < scale) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int
             -> Int
             -> V.Vector (V.Vector (UV.Vector Int))
             -> V.Vector (V.Vector Simplex)
             -> V.Vector (V.Vector Simplex)
      combos i max sc result =
        if i == max then
          --don't record boundary indices for edges
          (V.map (\s -> (s, UV.empty)) $ V.last sc) `V.cons` result
        else
          let i1        = i + 1
              --sc is in reverse order, so sc V.! i1 is the array of simplices one dimension lower
              current :: V.Vector (UV.Vector Int)
              current   = sc V.! i
              next :: V.Vector (UV.Vector Int)
              next      = sc V.! i1
              len       = V.length next
              allCombos :: V.Vector (V.Vector (UV.Vector Int))
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos :: V.Vector (UV.Vector Int)
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by
              --adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices :: V.Vector (UV.Vector Int)
              indices   = parMapVec (UV.convert . (V.map (\face ->
                            len + (elemIndexUnsafe face uCombos)))) allCombos
          in combos i1 max (replaceElem i1 (next
               V.++ uCombos) sc) $ (V.zip current indices) `V.cons` result

      fstmc2 = fst maxCliques - 2
  in
    --if there are no maximal cliques, the complex is just a bunch of points
    if fstmc2 == (-1) then (numVerts, V.empty)
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

-- * Homology
{--
--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundariesBool :: SimplicialComplex -> BMatrix
makeEdgeBoundariesBool sc =
  let mat =
        V.map (\edge ->
          V.map (\vert -> vert == V.head edge || vert == V.last edge) $ 0 `range` (fst sc - 1))
            $ V.map fst $ V.head $ snd sc
  in
    case transposeMat mat of
      Just m  -> m
      Nothing -> error "Persistence.SimplicialComplex.makeEdgeBoundariesBool. This is a bug. Please email the Persistence maintainers."

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
makeSimplexBoundaryBool :: Int -> SimplicialComplex -> (Vector Int, Vector Int) -> Vector Bool
makeSimplexBoundaryBool dim simplices (simplex, indices) =
  mapWithIndex (\i s -> V.elem i indices) (V.map fst $ (snd simplices) V.! (dim - 2))

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorBool :: Int -> SimplicialComplex -> BMatrix
makeBoundaryOperatorBool dim sc =
  let mat = V.map (makeSimplexBoundaryBool dim sc) $ (snd sc) V.! (dim - 1)
  in
    case transposeMat mat of
      Just m  -> m
      Nothing -> error "Persistence.SimplicialComplex.makeBoundaryOperatorBool. This is a bug. Please email the Persistence maintainers."

--makes all the boundary operators
makeBoundaryOperatorsBool :: SimplicialComplex -> Vector BMatrix
makeBoundaryOperatorsBool sc =
  let dim = getDim sc
      calc i
        | i > dim   = V.empty
        | i == 1    = (makeEdgeBoundariesBool sc) `V.cons` (calc 2)
        | otherwise = (makeBoundaryOperatorBool i sc) `V.cons` (calc (i + 1))
  in calc 1

{- |
  The nth index of the output corresponds to the nth Betti number.

  The zeroth Betti number is the number of connected components, the 1st is the number of tunnels/  punctures, the 2nd is the number of hollow volumes, and so on for higher-dimensional holes.
-}
bettiNumbers :: SimplicialComplex -> [Int]
bettiNumbers sc =
  let dim      = (getDim sc) + 1
      boundOps = makeBoundaryOperatorsBool sc
      --dimension of image paired with dimension of kernel
      ranks    = (0, V.length $ V.head boundOps)
                   `V.cons` (V.map (\op -> let rank = rankBool op
                     in (rank, (V.length $ V.head op) - rank)) boundOps)
      calc 1   = [(snd $ ranks V.! 0) - (fst $ ranks V.! 1)]
      calc i   =
        let i1 = i - 1
        in
          if i == dim then (snd $ V.last ranks):(calc i1)
          else ((snd $ ranks V.! i1) - (fst $ ranks V.! i)):(calc i1)
  in
    if V.null $ snd sc then [fst sc]
    else L.reverse $ calc dim

-- | Calculates all of the Betti numbers in parallel.
bettiNumbersPar :: SimplicialComplex -> [Int]
bettiNumbersPar sc =
  let dim      = (getDim sc) + 1
      boundOps = makeBoundaryOperatorsBool sc
      --dimension of image paired with dimension of kernel
      ranks    = (0, V.length $ V.head boundOps)
                   `V.cons` (parMapVec (\op -> let rank = rankBool op
                     in (rank, (V.length $ V.head op) - rank)) boundOps)
      calc 1   = [(snd $ ranks V.! 0) - (fst $ ranks V.! 1)]
      calc i   =
        let i1 = i - 1
        in
          if i == dim then evalPar (snd $ V.last ranks) (calc i1) --see Util for evalPar
          else evalPar ((snd $ ranks V.! i1) - (fst $ ranks V.! i)) (calc i1)
  in
    if V.null $ snd sc then [fst sc]
    else L.reverse $ calc dim

--gets the first boundary operator
--because edges don't need to point to their faces
makeEdgeBoundariesInt :: SimplicialComplex -> IMatrix
makeEdgeBoundariesInt sc =
  let mat =
        V.map (\e -> let edge = fst e in
            replaceElem (V.head edge) (-1) $
              replaceElem (V.last edge) 1 $
                V.replicate (fst sc) 0) $
                  V.head $ snd sc
  in
    case transposeMat mat of
      Just m  -> m
      Nothing -> error "Persistence.SimplicialComplex.makeEdgeBoundariesInt. This is a bug. Please email the Persistence maintainers."

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is the simplices of one dimension lower
--second argument is the simplex paired with the indices of its faces
makeSimplexBoundaryInt :: Vector (Vector Int, Vector Int) -> (Vector Int, Vector Int) -> Vector Int
makeSimplexBoundaryInt simplices (_, indices) =
  let calc1 ixs result =
        if V.null ixs then result
        else calc2 (V.tail ixs) $ replaceElem (V.head ixs) (-1) result
      calc2 ixs result =
        if V.null ixs then result
        else calc1 (V.tail ixs) $ replaceElem (V.head ixs) 1 result
  in calc1 indices $ V.replicate (V.length simplices) 0

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorInt :: Int -> SimplicialComplex -> IMatrix
makeBoundaryOperatorInt dim sc =
  let mat = V.map (makeSimplexBoundaryInt ((snd sc) V.! (dim - 2))) $ (snd sc) V.! (dim - 1)
  in
    case transposeMat mat of
      Just m  -> m
      Nothing -> error $ show mat

--makes all the boundary operators
makeBoundaryOperatorsInt :: SimplicialComplex -> Vector IMatrix
makeBoundaryOperatorsInt sc =
  let dim = getDim sc
      calc 1 = (makeEdgeBoundariesInt sc) `V.cons` (calc 2)
      calc i =
        if i > dim then V.empty
        else (makeBoundaryOperatorInt i sc) `V.cons` (calc $ i + 1)
  in calc 1

{- |
  Calculates all homology groups of the complex. An int list represents a single homology group.

  Specifically, an int list represents a direct product of cyclic groups - a 0 entry corresponds to the infinite cylic group (also known as the integers), and an entry of n corresponds to a cyclic group of order n (also known as the integers modulo n). The nth index of the output represents the nth homology group.

  If you don't have a rigorous understanding of what homology is, I recomend using BettiNumbers on large data-sets instead, as the output of those functions is more easily interprettable.
-}
simplicialHomology :: SimplicialComplex -> [[Int]]
simplicialHomology sc =
  let dim      = getDim sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 0   = [getUnsignedDiagonal $ normalFormInt (boundOps V.! 0)]
      calc i   =
        if i == dim then
          let op = V.last boundOps
          in (L.replicate ((V.length $ V.head op) - (rankInt op)) 0):(calc $ i - 1)
        else
          let i1 = i - 1
          in (getUnsignedDiagonal $ normalFormInt $ imgInKerInt (boundOps V.! i1) (boundOps V.! i)):(calc i1)
  in
    if L.null $ snd sc then [L.replicate (fst sc) 0]
    else L.reverse $ L.map (L.filter (/=1)) $ calc dim

-- | Same as simplicialHomology except it computes each of the groups in parallel and uses parallel matrix computations.
simplicialHomologyPar :: SimplicialComplex -> [[Int]]
simplicialHomologyPar sc =
  let dim      = getDim sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 0   = [getUnsignedDiagonal $ normalFormInt (boundOps V.! 0)]
      calc i   =
        if i == dim then
          let op = V.last boundOps
          in evalPar (L.replicate ((V.length $ V.head op) - (rankInt op)) 0) $ calc $ i - 1
        else
          let i1 = i - 1
          in evalPar (getUnsignedDiagonal $ normalFormIntPar $ --see Util for evalPar
            imgInKerIntPar (boundOps V.! i1) (boundOps V.! i)) $ calc i1
  in
    if L.null $ snd sc then [L.replicate (fst sc) 0]
    else L.reverse $ L.map (L.filter (/=1)) $ calc dim
--}