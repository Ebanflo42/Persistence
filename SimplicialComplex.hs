module SimplicialComplex
  ( SimplicialComplex
  , sc2String
  , getDimension
  , makeVRComplex
  , makeBoundaryOperatorsInt
  , calculateHomologyInt
  , calculateHomologyIntPar
  , makeBoundaryOperatorsBool
  , calculateHomologyBool
  , calculateHomologyBoolPar
  ) where

import Util
import Matrix
import MaximalCliques
import Data.List as L
import Data.Vector as V
import Data.IntSet as S
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------
Simplicial complexes are represented as a pair. The first component is an integer
indicating the number of vertices (might remove that) and the second is a list
of arrays of simplices whose dimension is given by the index in the list +2.
The simplices are represented as an array of their vertices with an array of
indices pointing to their faces in the previous entry of the list.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology
over both the integers and the integers modulo 2 (represented with booleans).

The Vietoris-Rips complex is constructed by first finding all maximal cliques of the data set given
the metric and scale (all arrras of points which fall within the scale of each other) and then
enumerating all the faces of the cliques.

Homology groups are represented by integer lists. An element being 0 in the list represents a factor
of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the
cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator.
First, the kernel of the nth boundary operator is found (in Matrix.hs) and its basis is arranged into the rows of a matrix.
Since the image of the (n+1)th boundary operator is its column space, it is left-multiplied by the kernel matrix
to project the basis of the image onto the basis of the kernel, a change of coordinates. Once this is done,
the Smith normal form of that matrix is computed so that we can see how the basis of one vector space fits into the other.
The diagonal of the Smith normal form represents the nth homology group.
--------------------------------------------------------------------------}

--CONSTRUCTION------------------------------------------------------------

--the first component of the pair is the number of vertices
--every element of the list is a vector of simplices whose dimension is given by the index +2
--a simplex is represented by a pair: the indices of its vertices and the indices of the faces in the previous entry of the list
--this is to speed up construction of the boundary operators
--the first entry in the list, the edges, do not point to their faces because that would be trivial
type SimplicialComplex = (Int, [Vector (Vector Int, Vector Int)])

sc2String :: SimplicialComplex -> String
sc2String (v, edges:simplices) =
  let showSimplex s     =
        '\n':(intercalate "\n" $ V.toList $ V.map show s)
      showAll sc =
        case sc of
          (s:ss) -> showSimplex s L.++ ('\n':(showAll ss))
          []     -> '\n':(show v) L.++ " vertices"
  in (intercalate "\n" $ V.toList $ V.map (show . fst) edges) L.++ ('\n':(showAll simplices))

getDimension :: SimplicialComplex -> Int
getDimension = L.length . snd

--makes the Vietoris-Rips complex given a scale, metric, and data set
--uses Bron-Kerbosch algorithm to find maximal cliques and then enumerates faces
makeVRComplex :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> SimplicialComplex
makeVRComplex scale metric dataSet =
  let numVerts = L.length dataSet
      organizeCliques dim simplices = --make a dataSet with an entry for every dimension
        case L.findIndex (\v -> (V.length v) /= dim) simplices of
          Just i  ->
            let diff = (V.length $ simplices !! i) - dim
            in
              if diff == 1 then (V.fromList $ L.take i simplices):(organizeCliques (dim - 1) $ L.drop i simplices)
              else (V.fromList $ L.take i simplices):((L.replicate (diff - 1) V.empty)
                L.++ (organizeCliques (dim - 1) $ L.drop i simplices))
          Nothing -> [V.fromList simplices]
      makePair simplices = --pair the organized dataSet of maximal cliques with its dimension
        let dim = V.length $ L.head simplices
        in (dim, organizeCliques dim simplices)
      maxCliques =
        makePair $ sortVecs $ L.map V.fromList $
          L.filter (\c -> L.length c > 1) $ getMaximalCliques (\i j -> metric (dataSet !! i) (dataSet !! j) < scale) [0..numVerts - 1]
      combos i max sc result =
        if i == max then --don't need to record boundary indices for edges
          (V.map (\s -> (s, V.empty)) $ L.last sc):result
        else
          let i1        = i + 1
              current   = sc !! i
              next      = sc !! i1
              len       = V.length next
              allCombos = V.map getCombos current
              uCombos   = bigU allCombos
              indices   = V.map (V.map (\face -> len + (V.head $ V.elemIndices face uCombos))) allCombos
          in combos i1 max (replaceElemList i1 (next V.++ uCombos) sc) $ (V.zip current indices):result
  in (L.length dataSet, combos 0 (fst maxCliques - 2) (snd maxCliques) [])

--INTEGER HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their subsimplices)

makeEdgeBoundariesInt :: SimplicialComplex -> IMatrix
makeEdgeBoundariesInt sc =
  transposeMat $
    V.map (\e -> let edge = fst e in
      replaceElem (V.head edge) (-1) $
        replaceElem (V.last edge) 1 $
          V.replicate (fst sc) 0) $
            L.head $ snd sc

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
makeBoundaryOperatorInt dim sc = transposeMat $ V.map (makeSimplexBoundaryInt ((snd sc) !! (dim - 2))) $ (snd sc) !! (dim - 1)

--makes all the boundary operators
makeBoundaryOperatorsInt :: SimplicialComplex -> Vector IMatrix
makeBoundaryOperatorsInt sc =
  let dim = getDimension sc
      calc 1 = (makeEdgeBoundariesInt sc) `cons` (calc 2)
      calc i =
        if i > dim then V.empty
        else (makeBoundaryOperatorInt i sc) `cons` (calc $ i + 1)
  in calc 1

--calculates all homology groups of the complex
calculateHomologyInt :: SimplicialComplex -> [[Int]]
calculateHomologyInt sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 1   = [getDiagonal $ normalFormInt $ imgInKerInt (boundOps ! 0) (boundOps ! 1)]
      calc i   =
        if i == dim then
          (L.replicate (V.length $ kernelInt$ V.last boundOps) 0):(calc $ i - 1)
        else
          let i1 = i - 1
          in (getDiagonal $ normalFormInt $
            imgInKerInt (boundOps ! i1) (boundOps ! i)):(calc i1)
  in calc dim

--calculates all homology groups of the complex in parallel using parallel matrix functions
calculateHomologyIntPar :: SimplicialComplex -> [[Int]]
calculateHomologyIntPar sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsInt sc
      calc 1   = [getDiagonal $ normalFormIntPar $ imgInKerIntPar (boundOps ! 0) (boundOps ! 1)]
      calc i   =
        if i == dim then
          evalPar (L.replicate (V.length $ kernelIntPar $ V.last boundOps) 0) $ calc $ i - 1
        else
          let i1 = i - 1
          in evalPar (getDiagonal $ normalFormIntPar $ --see Util for evalPar
            imgInKerIntPar (boundOps ! i1) (boundOps ! i)) $ calc i1
  in calc dim

--BOOLEAN HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundariesBool :: SimplicialComplex -> BMatrix
makeEdgeBoundariesBool sc =
  transposeMat $ V.map (\edge ->
    V.map (\vert -> vert == V.head edge || vert == V.last edge) $ 0 `range` (fst sc - 1)) $
      V.map fst $ L.head $ snd sc

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
makeSimplexBoundaryBool :: Int -> SimplicialComplex -> (Vector Int, Vector Int) -> Vector Bool
makeSimplexBoundaryBool dim simplices (simplex, indices) =
  mapWithIndex (\i s -> V.elem i indices) (V.map fst $ (snd simplices) !! (dim - 2))

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorBool :: Int -> SimplicialComplex -> BMatrix
makeBoundaryOperatorBool dim sc = transposeMat $ V.map (makeSimplexBoundaryBool dim sc) $ (snd sc) !! (dim - 1)

--makes all the boundary operators
makeBoundaryOperatorsBool :: SimplicialComplex -> Vector BMatrix
makeBoundaryOperatorsBool sc =
  let dim = getDimension sc
      calc i
        | i > dim   = V.empty
        | i == 1    = (makeEdgeBoundariesBool sc) `cons` (calc 2)
        | otherwise = (makeBoundaryOperatorBool i sc) `cons` (calc (i + 1))
  in calc 1

--calculate the ranks of all homology groups
calculateHomologyBool :: SimplicialComplex -> [Int]
calculateHomologyBool sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsBool sc
      calc 1   = [rankBool $ imgInKerBool (boundOps ! 0) (boundOps ! 1)]
      calc i   =
        if i == dim then
          ((V.length $ V.head $ V.last boundOps) - (rankBool $ V.last boundOps)):(calc $ i - 1)
        else
          let i1 = i - 1
          in (rankBool $ imgInKerBool (boundOps ! i1) (boundOps ! i)):(calc i1)
  in calc dim

--calculate ranks of all homology groups in parallel
calculateHomologyBoolPar :: SimplicialComplex -> [Int]
calculateHomologyBoolPar sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsBool sc
      ranks    = --dimension of image paired with dimension of kernel
        (0, V.length $ V.head boundOps) `cons`
          (parMapVec (\op -> let rank = rankBool op in (rank, (V.length $ V.head op) - rank)) $ V.tail boundOps)
      calc 0   = [(snd $ V.head ranks) - (fst $ ranks ! 1)]
      calc i   =
        let i1 = i - 1
        in
          if i == dim then evalPar (snd $ V.last ranks) (calc i1) --see Util for evalPar
          else evalPar ((snd $ ranks ! i) - (fst $ ranks ! i1)) (calc i1)
  in calc dim