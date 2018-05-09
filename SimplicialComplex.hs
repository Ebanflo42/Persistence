{- |
Module     : Persistence.SimplicialComplex
Copyright  : (c) Eben Cowley, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module provides functions for constructing neighborhood graphs, clique complexes, Vietoris-Rips complexes, as well as the computation of Betti numbers.

An important thing to know about this module is the difference between "fast" and "light" functions. Fast functions encode the metric in a 2D vector, so that distances don't need to be computed over and over again and can instead be accessed in constant time. Unfortunately, this takes O(n^2) memory so I would strongly recomend against using it for larger data sets; this is why "light" functions exist.

A neighborhood graph is a graph where an edge exists between two vertices if and only if the vertices fall within a given distance of each other. Graphs here are more of a helper data type for constructing filtrations, which is why they have a somewhat odd representation.

The clique complex of a graph is a simplicial complex whose simplices are the complete subgraphs of the given graph. The Vietoris-Rips complex is the clique complex of the neighborhood graph.

Betti numbers measure the number of n-dimensional holes in a given simplicial complex. The 0th Betti number is the number of connected components, or clusters; the 1st Betti number is the number of loops or tunnels; then 2nd Betti number measures voids or hollow volumes; and if you have high-dimensional data, you might have higher Betti numbers representing the number of high-dimensional holes.

-}

module SimplicialComplex
  ( SimplicialComplex
  , Graph
  , sc2String
  , getDim
  , makeNbrhdGraph
  , makeCliqueComplex
  , makeVRComplexFast
  , makeVRComplexLight
  , simplicialHomology
  , simplicialHomologyPar
  , bettiNumbers
  , bettiNumbersPar
  ) where

import Util
import Matrix

import Data.List as L
import Data.Vector as V
import Data.IntSet as S
import Control.Parallel.Strategies
import Data.Algorithm.MaximalCliques

--BASIC STUFF-------------------------------------------------------------

{- |
  The first component of the pair is the number of vertices.
  The second component is a vector whose entries are vectors of simplices of the same dimension.
  The dimension starts at 1 because there is no need to store individual vertices.
  A simplex is represented as a pair: the vector of its vertices in the original data set from which the complex was constructed,
  and the vector of the indices of the faces in the next lowest dimension. Edges are the exception - they do not store reference to their
  faces because it would be redundant with their vertices.
-}
type SimplicialComplex = (Int, Vector (Vector (Vector Int, Vector Int)))

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

-- | Get the dimension of the highest dimensional simplex (constant time).
getDim :: SimplicialComplex -> Int
getDim = L.length . snd

{- |
  This represents the (symmetric) adjacency matrix of some undirected graph. The type `a` is whatever distance is in your data analysis regime.
  The reason graphs are represented like this is because their main function is too speed up the construction of simplicial complexes and filtrations.
  If the clique complex of this graph were to be constructed, only the adjacencies would matter. But if a filtration is constructed all the distances
  will be required over and over again - this allows them to be accessed in constant time.
-}
type Graph a = Vector (Vector (a, Bool))

{- |
  The first argument is a scale, the second is a metric, and the third is the data.
  This function records the distance between every element of the data and whether or not it is smaller than the given scale.
-}
makeNbrhdGraph :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> Graph a
makeNbrhdGraph scale metric dataSet =
  let vector      = V.fromList dataSet
  in V.map (\x -> V.map (\y -> let d = metric x y in (d, d <= scale)) vector) vector

--CONSTRUCTION------------------------------------------------------------

{- |
  Makes a simplicial complex where the simplices are the complete subgraphs (cliques) of the given graph.
  Mainly a helper function for makeVRComplexFast, but it might be useful if you happen to have a graph you want to analyze.
  This utilizes any available processors in parallel because the construction is quite expensive.
-}
makeCliqueComplex :: Graph a -> SimplicialComplex
makeCliqueComplex graph =
  let numVerts = V.length graph
      --make a list with an entry for every dimension of simplices
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        let helper = biFilter (\simplex -> i == V.length simplex) cliques --find the simplices with the given number of vertices
        in (fst helper):(organizeCliques (i - 1) $ snd helper) --append them to the next iteration of the function

      makePair simplices = --pair the organized maximal cliques with the dimension of the largest clique
        case simplices of
          (x:_) ->
            let dim = V.length x
            in (dim, organizeCliques dim $ V.fromList simplices)
          []    -> (1, []) --if there are no maximal cliques this acts as a flag so that the algorithm doesn't try to generate all the other simplices

      maxCliques :: (Int, Vector (Vector (Vector Int)))
      maxCliques = --find all maximal cliques and sort them from largest to smallest (excludes maximal cliques which are single points)
        (\(x, y) -> (x, V.fromList y)) $ makePair $ sortVecs $ L.map V.fromList $
          L.filter (\c -> L.length c > 1) $ getMaximalCliques (\i j -> snd $ graph ! i ! j) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int -> Int -> Vector (Vector (Vector Int)) -> Vector (Vector (Vector Int, Vector Int)) -> Vector (Vector (Vector Int, Vector Int))
      combos i max sc result =
        if i == max then
          (V.map (\s -> (s, V.empty)) $ V.last sc) `cons` result --don't record boundary indices for edges
        else
          let i1        = i + 1 --sc is in reverse order, so sc !! i1 is the array of simplices one dimension lower
              current   = sc ! i
              next      = sc ! i1
              len       = V.length next
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices   = parMapVec (V.map (\face -> len + (elemIndexUnsafe face uCombos))) allCombos
          in combos i1 max (replaceElem i1 (next V.++ uCombos) sc) $ (V.zip current indices) `cons` result

      fstmc2 = fst maxCliques - 2
  in
    if fstmc2 == (-1) then (numVerts, V.empty) --if there are no maximal cliques, the complex is just a bunch of points
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

{- |
  Constructs the Vietoris-Rips complex given a scale, metric, and data set.
  Also uses O(n^2) memory (where n is the number of data points) for a graph storing all the distances between data points.
  This utilizes any available processors in parallel because the construction is quite expensive.
-}
makeVRComplexFast :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> (SimplicialComplex, Graph a)
makeVRComplexFast scale metric dataSet =
  let graph = makeNbrhdGraph scale metric dataSet
      sc    = makeCliqueComplex graph
  in (sc, graph)

{- |
  Constructs the Vietoris-Rips complex given a scale, metric, and data set.
  This utilizes any available processors in parallel because the construction is quite expensive.
-}
makeVRComplexLight :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> SimplicialComplex
makeVRComplexLight scale metric dataSet =
  let numVerts = L.length dataSet
      vector   = V.fromList dataSet

      --make a list with an entry for every dimension of simplices
      organizeCliques 1 _       = []
      organizeCliques i cliques =
        let helper = biFilter (\simplex -> i == V.length simplex) cliques --find the simplices with the given number of vertices
        in (fst helper):(organizeCliques (i - 1) $ snd helper) --append them to the next iteration of the function

      makePair simplices = --pair the organized maximal cliques with the dimension of the largest clique
        case simplices of
          (x:_) ->
            let dim = V.length x
            in (dim, organizeCliques dim $ V.fromList simplices)
          []    -> (1, []) --if there are no maximal cliques this acts as a flag so that the algorithm doesn't try to generate all the other simplices

      maxCliques :: (Int, Vector (Vector (Vector Int)))
      maxCliques = --find all maximal cliques and sort them from largest to smallest (excludes maximal cliques which are single points)
        (\(x, y) -> (x, V.fromList y)) $ makePair $ sortVecs $ L.map V.fromList $
          L.filter (\c -> L.length c > 1) $ getMaximalCliques (\i j -> metric (vector ! i) (vector ! j) <= scale) [0..numVerts - 1]

      --generates faces of simplices and records the boundary indices
      combos :: Int -> Int -> Vector (Vector (Vector Int)) -> Vector (Vector (Vector Int, Vector Int)) -> Vector (Vector (Vector Int, Vector Int))
      combos i max sc result =
        if i == max then
          (V.map (\s -> (s, V.empty)) $ V.last sc) `cons` result --don't record boundary indices for edges
        else
          let i1        = i + 1 --sc is in reverse order, so sc !! i1 is the array of simplices one dimension lower
              current   = sc ! i
              next      = sc ! i1
              len       = V.length next
              allCombos = V.map getCombos current --get all the faces of every simplex
              uCombos   = bigU allCombos --union of faces
              --the index of the faces of each simplex can be found by adding the number of (n-1)-simplices to the index of each face in the union of faces
              indices   = parMapVec (V.map (\face -> len + (elemIndexUnsafe face uCombos))) allCombos
          in combos i1 max (replaceElem i1 (next V.++ uCombos) sc) $ (V.zip current indices) `cons` result

      fstmc2 = fst maxCliques - 2
  in
    if fstmc2 == (-1) then (numVerts, V.empty) --if there are no maximal cliques, the complex is just a bunch of points
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in (numVerts, sc)

--INTEGER HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their faces)
makeEdgeBoundariesInt :: SimplicialComplex -> IMatrix
makeEdgeBoundariesInt sc =
  transposeMat $
    V.map (\e -> let edge = fst e in
      replaceElem (V.head edge) (-1) $
        replaceElem (V.last edge) 1 $
          V.replicate (fst sc) 0) $
            V.head $ snd sc

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
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
makeBoundaryOperatorInt dim sc = transposeMat $ V.map (makeSimplexBoundaryInt ((snd sc) ! (dim - 2))) $ (snd sc) ! (dim - 1)

--makes all the boundary operators
makeBoundaryOperatorsInt :: SimplicialComplex -> Vector IMatrix
makeBoundaryOperatorsInt sc =
  let dim = getDim sc
      calc 1 = (makeEdgeBoundariesInt sc) `cons` (calc 2)
      calc i =
        if i > dim then V.empty
        else (makeBoundaryOperatorInt i sc) `cons` (calc $ i + 1)
  in calc 1

{- |
  Calculates all homology groups of the complex. An int list represents a single homology group.
  Specifically, an int list represents a direct product of cyclic groups - a 0 entry corresponds to the
  infinite cylic group (also known as the integers), and an entry of n corresponds to a cyclic group of
  order n (also known as the integers modulo n). The nth index of the output represents the nth homology group.
-}
simplicialHomology :: SimplicialComplex -> [[Int]]
simplicialHomology sc =
  let dim      = getDim sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 0   = [getUnsignedDiagonal $ normalFormInt (boundOps ! 0)]
      calc i   =
        if i == dim then
          let op = V.last boundOps
          in (L.replicate ((V.length $ V.head op) - (rankInt op)) 0):(calc $ i - 1)
        else
          let i1 = i - 1
          in (getUnsignedDiagonal $ normalFormInt $ imgInKerInt (boundOps ! i1) (boundOps ! i)):(calc i1)
  in
    if L.null $ snd sc then [L.replicate (fst sc) 0]
    else L.reverse $ L.map (L.filter (/=1)) $ calc dim

-- | Same as simplicialHomology except it computes each of the groups in parallel and uses parallel matric functions.
simplicialHomologyPar :: SimplicialComplex -> [[Int]]
simplicialHomologyPar sc =
  let dim      = getDim sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 0   = [getUnsignedDiagonal $ normalFormInt (boundOps ! 0)]
      calc i   =
        if i == dim then
          let op = V.last boundOps
          in evalPar (L.replicate ((V.length $ V.head op) - (rankInt op)) 0) $ calc $ i - 1
        else
          let i1 = i - 1
          in evalPar (getUnsignedDiagonal $ normalFormIntPar $ --see Util for evalPar
            imgInKerIntPar (boundOps ! i1) (boundOps ! i)) $ calc i1
  in
    if L.null $ snd sc then [L.replicate (fst sc) 0]
    else L.reverse $ L.map (L.filter (/=1)) $ calc dim

--BOOLEAN HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundariesBool :: SimplicialComplex -> BMatrix
makeEdgeBoundariesBool sc =
  transposeMat $ V.map (\edge ->
    V.map (\vert -> vert == V.head edge || vert == V.last edge) $ 0 `range` (fst sc - 1)) $
      V.map fst $ V.head $ snd sc

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
makeSimplexBoundaryBool :: Int -> SimplicialComplex -> (Vector Int, Vector Int) -> Vector Bool
makeSimplexBoundaryBool dim simplices (simplex, indices) =
  mapWithIndex (\i s -> V.elem i indices) (V.map fst $ (snd simplices) ! (dim - 2))

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorBool :: Int -> SimplicialComplex -> BMatrix
makeBoundaryOperatorBool dim sc = transposeMat $ V.map (makeSimplexBoundaryBool dim sc) $ (snd sc) ! (dim - 1)

--makes all the boundary operators
makeBoundaryOperatorsBool :: SimplicialComplex -> Vector BMatrix
makeBoundaryOperatorsBool sc =
  let dim = getDim sc
      calc i
        | i > dim   = V.empty
        | i == 1    = (makeEdgeBoundariesBool sc) `cons` (calc 2)
        | otherwise = (makeBoundaryOperatorBool i sc) `cons` (calc (i + 1))
  in calc 1

{- |
  The nth index of the output corresponds to the nth Betti number.
  The zeroth Betti number is the number of connected components, the 1st is the number of tunnels/punctures,
  the 2nd is the number of hollow volumes, and so on for higher-dimensional holes.
-}
bettiNumbers :: SimplicialComplex -> [Int]
bettiNumbers sc =
  let dim      = (getDim sc) + 1
      boundOps = makeBoundaryOperatorsBool sc
      ranks    = --dimension of image paired with dimension of kernel
        (0, V.length $ V.head boundOps) `cons`
          (V.map (\op -> let rank = rankBool op in (rank, (V.length $ V.head op) - rank)) boundOps)
      calc 1   = [(snd $ ranks ! 0) - (fst $ ranks ! 1)]
      calc i   =
        let i1 = i - 1
        in
          if i == dim then (snd $ V.last ranks):(calc i1)
          else ((snd $ ranks ! i1) - (fst $ ranks ! i)):(calc i1)
  in
    if L.null $ snd sc then [fst sc]
    else L.reverse $ calc dim

-- | Calculates all of the Betti numbers in parallel.
bettiNumbersPar :: SimplicialComplex -> [Int]
bettiNumbersPar sc =
  let dim      = (getDim sc) + 1
      boundOps = makeBoundaryOperatorsBool sc
      ranks    = --dimension of image paired with dimension of kernel
        (0, V.length $ V.head boundOps) `cons`
          (parMapVec (\op -> let rank = rankBool op in (rank, (V.length $ V.head op) - rank)) boundOps)
      calc 1   = [(snd $ ranks ! 0) - (fst $ ranks ! 1)]
      calc i   =
        let i1 = i - 1
        in
          if i == dim then evalPar (snd $ V.last ranks) (calc i1) --see Util for evalPar
          else evalPar ((snd $ ranks ! i1) - (fst $ ranks ! i)) (calc i1)
  in
    if L.null $ snd sc then [fst sc]
    else L.reverse $ calc dim