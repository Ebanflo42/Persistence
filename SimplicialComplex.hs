module SimplicialComplex
  ( SimplicialComplex
  , sc2String
  , Graph
  , makeNbrhdGraph
  , getDim
  , makeVRComplexFast
  , makeVRComplexLight
  , makeBoundaryOperatorsInt
  , simplicialHomologyInt
  , simplicialHomologyIntPar
  , makeBoundaryOperatorsBool
  , simplicialHomologyBool
  , simplicialHomologyBoolPar
  ) where

{--OVERVIEW---------------------------------------------------------------

Simplicial complexes are represented as a pair. The first component is an integer indicating the number of vertices and the second is a list of arrays of simplices whose dimension is given by the index in the outer list +2.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology over both the integers and the integers modulo 2 (represented with booleans).

The Vietoris-Rips complex is constructed by first finding all maximal cliques of the data set given the metric and scale (all arrrays of points which fall within the scale of each other) and then enumerating all the faces of the cliques.

An important aspect of this module, as well as the main Persistence.hs file, is the existence of "light" vs. "fast" functions. Since the distance between data points needs to be repeatedly evaluated for the construction of the Vietoris-Rips complex, "fast" functions construct a square array of distances between every single point in the data so that the distance can be retrieved extremely quickly. Since the memory cost of this array is n^2 wrt the number of data points, this could require a tremendous amount of memory for a substantially large data set - this is why "light" functions don't use this optimization.

Integer homology groups are represented by integer lists. An element being 0 in the list represents a factor of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator. The boundary operators represented by rectangular 2D arrays.

For homology over the integers, one must first put the nth boundary operator in column eschelon form and perform the corresponding inverse row operations on the n+1th boundary operator. After this process is complete the column space of the rows of the n+1th corresponding to zero columns in the column eschelon form is the image of the n+1th represented in the basis of the kernel of the nth (See the Stanford paper). These are the two modules we need to quotient; to get the representation of the quotient as a direct product of cyclic groups we look at the diagonal of the Smith normal form of the afformentioned matrix.

Simplicial homology over F_2 (the field with 2 elements) is much simpler. The only information we could possibly need from any homology group is its dimension as an F_2 vector space. Since it is a quotient space, this is simply the number of n-simplices in the complex minus the rank of the nth boundary operator minus the rank of the n+1th boundary operator.

--------------------------------------------------------------------------}

import Util
import Matrix
import MaximalCliques
import Data.List as L
import Data.Vector as V
import Data.IntSet as S
import Control.Parallel.Strategies

--CONSTRUCTION------------------------------------------------------------

--the first component of the pair is the number of vertices
--every element of the list is a vector of simplices whose dimension is given by the index +2
--a simplex is represented by a pair: the indices of its vertices and the indices of the faces in the previous entry of the list
--this is to speed up construction of the boundary operators
--the first entry in the list, the edges, do not point to their faces because that would be trivial
type SimplicialComplex = (Int, Vector (Vector (Vector Int, Vector Int)))

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

getDim :: SimplicialComplex -> Int
getDim = L.length . snd

--structure for storing the distance between data points and whether or not there is a connection, used in "fast" functions
type Graph a = Vector (Vector (a, Bool))

--records the distance between every element of the data set and whether or not it is smaller than the given scale
makeNbrhdGraph :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> Graph a
makeNbrhdGraph scale metric dataSet =
  let vector      = V.fromList dataSet
  in V.map (\x -> V.map (\y -> let d = metric x y in (d, d <= scale)) vector) vector

--makes the Vietoris-Rips complex given a scale, metric, and data set
--uses Bron-Kerbosch algorithm to find maximal cliques and then enumerates faces
--uses parallelism by default because the construction is expensive
makeVRComplexFast :: (Ord a, Eq b) => a -> (b -> b -> a) -> [b] -> (SimplicialComplex, Graph a)
makeVRComplexFast scale metric dataSet =
  let numVerts = L.length dataSet
      graph    = makeNbrhdGraph scale metric dataSet

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
    if fstmc2 == (-1) then ((numVerts, V.empty), graph) --if there are no maximal cliques, the complex is just a bunch of points
    else
      let sc = combos 0 fstmc2 (snd maxCliques) V.empty
      in ((numVerts, sc), graph)

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

--gets the first boundary operator (because edges don't need to point to their subsimplices)

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

--calculates all homology groups of the complex
simplicialHomologyInt :: SimplicialComplex -> [[Int]]
simplicialHomologyInt sc =
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
    else L.map (L.filter (/=0)) $ calc dim

--calculates all homology groups of the complex in parallel using parallel matrix functions
simplicialHomologyIntPar :: SimplicialComplex -> [[Int]]
simplicialHomologyIntPar sc =
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
    else L.map (L.filter (/=0)) $ calc dim

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

--calculate the ranks of all homology groups
simplicialHomologyBool :: SimplicialComplex -> [Int]
simplicialHomologyBool sc =
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
    else calc dim

--calculate ranks of all homology groups in parallel
simplicialHomologyBoolPar :: SimplicialComplex -> [Int]
simplicialHomologyBoolPar sc =
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
    else calc dim