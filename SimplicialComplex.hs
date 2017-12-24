module SimplicialComplex
  ( SimplicialComplex
  , getDimension
  , makeNbrhdGraph
  , makeVRComplex
  , makeVRComplexFromGraph
  , calculateNthHomologyInt
  , calculateNthHomologyIntPar
  , calculateNthHomologyBool
  , calculateHomologyInt
  , calculateHomologyIntPar
  , calculateHomologyBool
  , calculateHomologyBoolPar
  ) where

import Util
import Matrix
import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

{--OVERVIEW---------------------------------------------------------------
Simplicial complexes are represented as linked lists of vectors of pairs of integer vectors.
Each index of the list represents all simplices of that dimension.
A simplex is represented by a pair - a vector with its veritces and a vector with the indices
of its faces in the next lowest index of the list.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology
over both the integers and the integers modulo 2 (represented with booleans).

The construction of the Vietoris-Rips complex has two steps when starting from a metric data set:
1) Construct the neighborhood graph, simply make an edge between any two points that fall within the given distance
2) Construct the clique complex of the neighborhood graph. This is far more complicated and the current algorithm is untested

Homology groups are represented by integer lists. An element being 0 in the list represents a factor
of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the
cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator.
First, the kernel of the nth boundary operator is found (in Matrix) and its basis is arranged into the rows of a matrix.
Since the image of the (n+1)th boundary operator is its column space, it is left-multiplied by the kernel matrix
to project the basis of the image onto the basis of the kernel, a change of coordinates. Once this is done,
the Smith normal form of that matrix is computed so that we can see how the basis of one vector space fits into the other.
The diagonal of the Smith normal form represents the nth homology group.
--}

--CONSTRUCTION------------------------------------------------------------

--every element of the list is a vector of simplices whose dimension is given by the index
--the vertcies of the simplex are in the fst vector
--the indices of its subsimplices in the next lowest list are in the snd vector, except for dimension 2
--vertices are not in the list, only a pair with a vector containing the number of vertices - 1 and an empty vector
type SimplicialComplex = [Vector (Vector Int, Vector Int)]

getDimension :: SimplicialComplex -> Int
getDimension simplices = L.length simplices - 1

--given a scale, a metric, and a data set this constructs a graph out of the points in the data set
--two points share an edge iff the distance between them is less than the scale
makeNbrhdGraph :: Ord a => a -> (b -> b -> a) -> [b] -> SimplicialComplex
makeNbrhdGraph scale metric list =
  let helper _ []     = empty
      helper i (x:xs) =
        let helper2 _ []     = empty
            helper2 j (y:ys) =
              if metric x y < scale then cons (cons i (cons j empty), empty) (helper2 (j + 1) ys)
              else helper2 (j + 1) ys in
        helper2 (i + 1) xs V.++ helper (i + 1) xs in
  (cons (cons (L.length list - 1) empty, empty) empty):[helper 0 list]

--this function is passed the dimension, a simplex, the rest of the simplices maybe differ by exactly one element
--if a simplex does differ from the argument by one point it searches for all other simplices differing by that point
--and tries to make a higher simplex out of them
checkAdjacentSimplices :: Int -> Int -> (Vector Int, Vector Int)
  -> Vector (Vector Int, Vector Int) -> Vector (Maybe Int)
    -> Vector (Vector Int, Vector Int) -> Vector (Vector Int, Vector Int)
checkAdjacentSimplices index dim simplex simplices adjacency result =
  if V.null adjacency then result
  else let x = V.head adjacency; xs = V.tail adjacency in
    case x of
      Nothing -> checkAdjacentSimplices (index + 1) dim simplex simplices xs result
      Just v  ->
        let common = myfilterVec (\a -> a == x) xs
            len    = V.length $ one common in
        if len == dim then
          checkAdjacentSimplices (index + 1) dim simplex simplices (thr common)
            ((v `cons` (fst simplex), index `cons` (snd simplex)) `cons` result)
        else if len < dim then
          checkAdjacentSimplices (index + 1) dim simplex simplices xs result
        else error "Neighborhood graph was a multigraph, or this algorithm isn't working."

--given a dimension and all simplices of that dimension, finds simplices one dimension higher
findHigherSimplices :: Int -> Vector (Vector Int, Vector Int) -> Vector (Vector Int, Vector Int)
findHigherSimplices dim simplices =
  if V.null simplices then empty
  else let x = V.head simplices; xs = V.tail simplices in
    (checkAdjacentSimplices 0 dim x xs (V.map (diffByOneElem $ fst x) $ V.map fst xs) empty) V.++ (findHigherSimplices dim xs)

--given graph, constructs the clique complex
constructSimplices :: Int -> SimplicialComplex -> SimplicialComplex
constructSimplices dim result =
  let currentSimplices = L.last result in
  if V.null currentSimplices then L.init result --if there are no higher simplices to be found, return
  else constructSimplices (dim + 1) (result L.++ [findHigherSimplices dim currentSimplices])

--makes the Vietoris-Rips complex given a scale, metric, and data set
--may need to start dimension higher or lower, first arg of constructSimplices
makeVRComplex :: Ord a => a -> (b -> b -> a) -> [b] -> SimplicialComplex
makeVRComplex scale metric list =
  constructSimplices 2 $ makeNbrhdGraph scale metric list

makeVRComplexFromGraph :: SimplicialComplex -> SimplicialComplex
makeVRComplexFromGraph = constructSimplices 2

--INTEGER HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundaryInt :: SimplicialComplex -> IMatrix
makeEdgeBoundaryInt sc =
  let makeCoeff = \n -> if n `mod` 2 == 0 then 1 else -1
      verts     = range 0 $ V.head $ fst $ V.head $ L.head sc in --the number of vertices is stored in the only entry in the fst part of the head vector
  transposeMat $ V.map (\edge ->
    V.map (\vert ->
      if vert == V.head edge || vert == V.last edge then makeCoeff vert
      else 0) verts) $ V.map fst $ sc !! 1

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
makeSimplexBoundaryInt :: Int -> SimplicialComplex -> (Vector Int, Vector Int) -> Vector Int
makeSimplexBoundaryInt dim simplices (simplex, indices) =
  let makeCoeff s  =
        case findMissing s simplex of
          Just x -> if x `mod` 2 == 0 then 1 else -1
          Nothing -> error "Something went terribly wrong, SimplicialComplex.makeSimplexBoundaryInt" in
  mapWithIndex (\i s -> if existsVec i indices then makeCoeff s else 0) (V.map fst $ simplices !! (dim - 1))

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorInt :: Int -> SimplicialComplex -> IMatrix
makeBoundaryOperatorInt dim sc = transposeMat $ V.map (makeSimplexBoundaryInt dim sc) $ sc !! dim

--makes all the boundary operators
makeBoundaryOperatorsInt :: SimplicialComplex -> [IMatrix]
makeBoundaryOperatorsInt sc =
  let dim    = getDimension sc
      calc i =
        if i > dim then []
        else if i == 1 then (makeEdgeBoundaryInt sc) : (calc 2)
        else (makeBoundaryOperatorInt i sc) : (calc (i + 1)) in
  calc 1

--calculates nth homology group of the simplicial complex
calculateNthHomologyInt :: Int -> SimplicialComplex -> [Int]
calculateNthHomologyInt n sc =
  if n == 0 then
    getUnsignedDiagonal $ getSmithNormalFormInt $ makeEdgeBoundaryInt sc
  else
    let dim = getDimension sc
        boundOps =
          case n of
            x | x == dim -> (Nothing, Just $ makeBoundaryOperatorInt n sc)
            x | x > dim  -> (Nothing, Nothing)
            _            -> (Just $ makeBoundaryOperatorInt (n + 1) sc, Just $ makeBoundaryOperatorInt n sc) in
    case boundOps of
      (Nothing, Nothing) -> []
      (Nothing, Just mx) -> L.replicate (V.length $ findKernelInt mx) 0
      (Just m1, Just m2) -> getUnsignedDiagonal $ getSmithNormalFormInt $ multiply (findKernelInt m2) m1

--calculates nth homology group using parallel matrix functions
calculateNthHomologyIntPar :: Int -> SimplicialComplex -> [Int]
calculateNthHomologyIntPar n sc =
  if n == 0 then
    getUnsignedDiagonal $ getSmithNormalFormIntPar $ makeEdgeBoundaryInt sc
  else
    let dim = getDimension sc
        boundOps =
          case n of
            x | x == dim -> (Nothing, Just $ makeBoundaryOperatorInt n sc)
            x | x > dim  -> (Nothing, Nothing)
            _            -> (Just $ makeBoundaryOperatorInt (n + 1) sc, Just $ makeBoundaryOperatorInt n sc) in
    case boundOps of
      (Nothing, Nothing) -> []
      (Nothing, Just mx) -> L.replicate (V.length $ findKernelInt mx) 0
      (Just m1, Just m2) -> (getUnsignedDiagonal . getSmithNormalFormIntPar . (multiplyPar $ findKernelInt m2)) m1

--calculates all homology groups of the complex
calculateHomologyInt :: SimplicialComplex -> [[Int]]
calculateHomologyInt sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsInt sc
      calc i
        | i == 0    = (getUnsignedDiagonal $ getSmithNormalFormInt $ L.head boundOps) : (calc 1)
        | i == dim  = [L.replicate (V.length $ findKernelInt $ boundOps !! i) 0]
        | otherwise =
          (getUnsignedDiagonal $ getSmithNormalFormInt $ multiplyPar (findKernelInt (boundOps !! i)) (boundOps !! (i + 1))) : (calc (i + 1)) in
  calc 0

--calculates all homology groups of the complex in parallel using parallel matrix functions
calculateHomologyIntPar :: SimplicialComplex -> [[Int]]
calculateHomologyIntPar sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsInt sc
      calc i
        | i == 0    =
          let rest    = calc 1
              current = getUnsignedDiagonal $ getSmithNormalFormIntPar $ L.head boundOps in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r)
        | i == dim  = [L.replicate (V.length $ findKernelInt $ boundOps !! i) 0]
        | otherwise =
          let rest    = calc $ i + 1
              current = 
                getUnsignedDiagonal $ getSmithNormalFormIntPar $
                  multiplyPar (findKernelInt (boundOps !! i)) (boundOps !! (i + 1)) in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r) in
  calc 0

--BOOLEAN HOMOLOGY--------------------------------------------------------

--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundaryBool :: SimplicialComplex -> BMatrix
makeEdgeBoundaryBool sc =
  let verts     = range 0 $ V.head $ fst $ V.head $ L.head sc in --the number of vertices is stored in the only entry in the fst part of the head vector
  transposeMat $ V.map (\edge ->
    V.map (\vert -> vert == V.head edge || vert == V.last edge) verts) $ V.map fst $ sc !! 1

--gets the boundary coefficients for a simplex of dimension 2 or greater
--first argument is dimension of the simplex
--second argument is the simplicial complex
--third argument is the simplex paired with the indices of its faces
makeSimplexBoundaryBool :: Int -> SimplicialComplex -> (Vector Int, Vector Int) -> Vector Bool
makeSimplexBoundaryBool dim simplices (simplex, indices) =
  mapWithIndex (\i s -> existsVec i indices) (V.map fst $ simplices !! (dim - 1))

--makes boundary operator for all simplices of dimension 2 or greater
--first argument is the dimension of the boundary operator, second is the simplicial complex
makeBoundaryOperatorBool :: Int -> SimplicialComplex -> BMatrix
makeBoundaryOperatorBool dim sc = transposeMat $ V.map (makeSimplexBoundaryBool dim sc) $ sc !! dim

--makes all the boundary operators
makeBoundaryOperatorsBool :: SimplicialComplex -> [BMatrix]
makeBoundaryOperatorsBool sc =
  let dim    = getDimension sc
      calc i =
        if i > dim then []
        else if i == 1 then (makeEdgeBoundaryBool sc) : (calc 2)
        else (makeBoundaryOperatorBool i sc) : (calc (i + 1)) in
  calc 1

--calculates the nth homology group
calculateNthHomologyBool :: Int -> SimplicialComplex -> [Int]
calculateNthHomologyBool n sc =
  if n == 0 then
    L.map (\b -> if b then 1 else 0) $ getUnsignedDiagonal $ getSmithNormalFormBool $ makeEdgeBoundaryBool sc
  else
    let dim = getDimension sc
        boundOps =
          case n of
            x | x == dim -> (Nothing, Just $ makeBoundaryOperatorBool n sc)
            x | x > dim  -> (Nothing, Nothing)
            _            -> (Just $ makeBoundaryOperatorBool (n + 1) sc, Just $ makeBoundaryOperatorBool n sc) in
    case boundOps of
      (Nothing, Nothing) -> [] --both boundary operators are trivial if homology greater than the dimension of the complex is computed, so the homology group is trivial as well
      (Nothing, Just mx) -> L.replicate (V.length $ findKernelBool mx) 0
      (Just m1, Just m2) -> L.map (\b -> if b then 1 else 0) $ getUnsignedDiagonal $ getSmithNormalFormBool $ multiply (findKernelBool m2) m1

--calculate all homology groups
calculateHomologyBool :: SimplicialComplex -> [[Int]]
calculateHomologyBool sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsBool sc
      calc i
        | i == 0    =
          (L.map (\b -> if b then 1 else 0) $
            getUnsignedDiagonal $ getSmithNormalFormBool $
              L.head boundOps) : (calc 1)
        | i == dim  = [L.replicate (V.length $ findKernelBool $ boundOps !! i) 0]
        | otherwise =
          (L.map (\b -> if b then 1 else 0) $
            getUnsignedDiagonal $ getSmithNormalFormBool $
              multiply (findKernelBool (boundOps !! i)) (boundOps !! (i + 1)))
                : (calc (i + 1)) in
  calc 0

--calculate all homology groups in parallel
calculateHomologyBoolPar :: SimplicialComplex -> [[Int]]
calculateHomologyBoolPar sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperatorsBool sc
      calc i
        | i == 0    =
          let rest    = calc 1
              current =
                L.map (\b -> if b then 1 else 0) $
                  getUnsignedDiagonal $ getSmithNormalFormBool $ L.head boundOps in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r)
        | i == dim  = [L.replicate (V.length $ findKernelBool $ boundOps !! i) 0]
        | otherwise =
          let rest    = calc $ i + 1
              current =
                L.map (\b -> if b then 1 else 0) $
                  getUnsignedDiagonal $ getSmithNormalFormBool $
                    multiply (findKernelBool (boundOps !! i)) (boundOps !! (i + 1)) in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r) in
  calc 0