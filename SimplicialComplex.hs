module SimplicialComplex
  ( getSimplices
  , getBoundaries
  , getDimension
  , isMod2SC
  , biggestSimplices
  , nDimensionalSimplices
  , makeNbrhdGraph
  , makeVRComplex
  , makeVRComplexFromGraph
  , getEdgeBoundary
  , getSimplexBoundary
  , getBoundaryOperator
  , makeBoundaryOperators
  , calculateNthHomology
  , calculateNthHomologyPar
  , calculateHomology
  , calculateHomologyPar
  ) where

import Util
import Matrix
import Data.List
import Control.Parallel.Strategies

--every element of the first list is a list of simplices whose dimension is given by the index
--every simplex is paired with a list of indices pointing to its faces
--second list is a list of boundary operators, the boolean is whether or not the homology is being calculated over F_2
data SimplicialComplex a = SimplicialComplex [[([a], [Int])]] [Matrix a] Bool

getSimplices (SimplicialComplex simplices _ _) = simplices
getBoundaries (SimplicialComplex _ matrices _) = matrices
getDimension (SimplicialComplex simplices _ _) = length simplices
isMod2SC (SimplicialComplex _ _ modulo2)         = modulo2

biggestSimplices :: Integral a => SimplicialComplex a -> [([a], [Int])]
biggestSimplices (SimplicialComplex simplices _ _) = last simplices

nDimensionalSimplices :: Integral a => Int -> SimplicialComplex a -> [([a], [Int])]
nDimensionalSimplices n sc = (getSimplices sc) !! n

--given a scale, a metric, and a data set this constructs a graph out of the points in the data set
--two points share an edge iff the distance between them is less than the scale
makeNbrhdGraph :: Ord a => a -> (b -> b -> a) -> [b] -> [[([Int], [Int])]]
makeNbrhdGraph scale metric list =
  let helper _ []     = []
      helper i (x:xs) =
        let helper2 _ []     = []
            helper2 j (y:ys) =
              if metric x y < scale then ([i, j], []) : (helper2 (j + 1) ys)
              else (helper2 (j + 1) ys) in
        helper2 (i + 1) xs in
  (map (\n -> ([n], [])) [0..length list - 1]) : [helper 0 list]

--this function is passed the dimension, a simplex, the rest of the simplices maybe differ by exactly one element
--if a simplex does differ from the argument by one point it searches for all other simplices differing by that point
--and tries to make a higher simplex out of them
checkAdjacentSimplices :: Int -> Int -> ([Int], Int) -> [([Int], Int)] -> [Maybe Int] -> [([Int], [Int])] -> [([Int], [Int])]
checkAdjacentSimplices index dim simplex simplices adjacency result =
  case adjacency of
    []               -> result
    (Nothing:rest)   ->
      checkAdjacentSimplices (index + 1) dim simplex simplices rest result
    ((Just x):rest)  ->
      let common = myfilter (\a -> a == Just x) rest
          len    = length $ one common in
      if len == dim then
        checkAdjacentSimplices (index + 1) dim simplex simplices (thr common)
          ((x:(fst simplex), (snd simplex):(map (\i -> i + index) $ two common)):result)
      else if len < dim then
        checkAdjacentSimplices (index + 1) dim simplex simplices rest result
      else error "Neighborhood graph was a multigraph, or this algorithm isn't working."

--given a dimension and all simplices of that dimension, finds simplices one dimension higher
findHigherSimplices :: Int -> [([Int], Int)] -> [([Int], [Int])]
findHigherSimplices dim simplices =
  case simplices of
    []     -> []
    (x:xs) ->
      (checkAdjacentSimplices 0 dim x xs (map (diffByOneElem $ fst x) $ map fst xs) []) ++ (findHigherSimplices dim xs)

--given graph, constructs the clique complex
constructSimplices :: Int -> [[([Int], [Int])]] -> [[([Int], [Int])]]
constructSimplices dim result =
  let currentSimplices = last result in
  case currentSimplices of
    [] -> init result --if there are no higher simplices to be found, return
    _  ->
      constructSimplices (dim + 1) (result ++ [findHigherSimplices dim (mapWithIndex (\i e -> (e, i)) $ map fst currentSimplices)])

--makes the Vietoris-Rips complex given a scale, metric, and data set
--may need to start dimension higher or lower, first arg of constructSimplices
makeVRComplex :: Ord a => a -> (b -> b -> a) -> [b] -> Bool -> SimplicialComplex Int
makeVRComplex scale metric list =
  SimplicialComplex (constructSimplices 2 $ makeNbrhdGraph scale metric list) []

makeVRComplexFromGraph :: [[([Int], [Int])]] -> Bool -> SimplicialComplex Int
makeVRComplexFromGraph graph = SimplicialComplex (constructSimplices 2 graph) []

--gets the first boundary operator (because edges don't need to point to their subsimplices)
getEdgeBoundary :: Integral a => SimplicialComplex a -> Matrix a
getEdgeBoundary (SimplicialComplex simplices _ isMod2SC) =
  let makeCoeff = \n -> if isMod2SC || n `mod` 2 == 0 then 1 else -1 in
  initializeMatrix isMod2SC $ transpose $
    map (\edge ->
      map (\vert -> let v = head vert in
                    if v == head edge || v == last edge then makeCoeff v
                    else 0) $ map fst $ head simplices) $ map fst (simplices !! 1)

--gets the boundary coefficients for a simplex of dimension 2 or greater
getSimplexBoundary :: Integral a => Int -> SimplicialComplex a -> ([a], [Int]) -> [a]
getSimplexBoundary dim (SimplicialComplex simplices _ isMod2SC) (simplex, indices) =
  let makeCoeff s  =
        if isMod2SC then 1
        else if (findMissing s simplex) `mod` 2 == 0 then 1
        else -1 in
  mapWithIndex (\i s -> if exists i indices then makeCoeff s else 0) (map fst $ simplices !! (dim - 1))

--makes boundary operator for all simplices of dimension 2 or greater
getBoundaryOperator :: Integral a => Int -> SimplicialComplex a -> Matrix a
getBoundaryOperator dim sc =
  initializeMatrix
    (SimplicialComplex.isMod2SC sc) $ transpose $
      (map (SimplicialComplex.getSimplexBoundary dim sc) $ (getSimplices sc) !! dim)

--makes all the boundary operators, should always be called before calculating homology
makeBoundaryOperators :: Integral a => SimplicialComplex a -> SimplicialComplex a
makeBoundaryOperators sc =
  let dim    = getDimension sc
      calc i =
        if i > dim then []
        else if i == 1 then (getEdgeBoundary sc) : (calc 2)
        else (getBoundaryOperator i sc) : (calc (i + 1)) in
  SimplicialComplex (getSimplices sc) (calc 1) (SimplicialComplex.isMod2SC sc)

calculateNthHomology :: Integral a => Int -> SimplicialComplex a -> [a]
calculateNthHomology n sc =
  let boundOps = getBoundaries sc
      parN1    =
        if n == (getDimension sc) then Nothing --boundary operators higher than the dimension of the complex always return identity
        else Just $ boundOps !! n in --otherwise select from list of matrices
  case parN1 of
    Nothing ->
      let kernel = findKernel (boundOps !! n) in --rows of this matrix form a basis for the kernel of nth boundary operator
      --if the image of the boundary is the identity return the infinite cyclic group to the power of the dimension of the kernel
      replicate (length $ getElems kernel) 0
    Just m  ->
      if n == 0 then --boundary of vertices is zero so just quotient space of vertices by image of edge boundary operator
        getUnsignedDiagonal $ getSmithNormalForm m
      else let kernel = findKernel (boundOps !! n) in
        (getUnsignedDiagonal . getSmithNormalForm . (multiply kernel)) m
        --otherwise multiply the image by the kernel matrix to get the project the vectors in the image onto the ones in the kernel
{--}
calculateNthHomologyPar :: Integral a => Int -> SimplicialComplex a -> [a]
calculateNthHomologyPar n sc =
  let boundOps = getBoundaries sc
      parN1    =
        if n == (getDimension sc) then Nothing
        else Just $ boundOps !! n
      kernel  =
        if n == 0 then Nothing
        else Just $ findKernel (boundOps !! n) in
  case parN1 of
    Nothing ->
      let kernel = findKernel (boundOps !! n) in
      replicate (length $ getElems kernel) 0
    Just m  ->
      if n == 0 then getUnsignedDiagonal $ getSmithNormalFormPar m
      else let kernel = findKernel (boundOps !! n) in
        (getUnsignedDiagonal . getSmithNormalFormPar . (multiply kernel)) m
--}
calculateHomology :: Integral a => SimplicialComplex a -> [[a]]
calculateHomology sc =
  let calc i =
        if i > getDimension sc then []
        else (calculateNthHomology i sc) : (calc $ i + 1) in
  calc 0
{--}
calculateHomologyPar :: Integral a => SimplicialComplex a -> [[a]]
calculateHomologyPar sc =
  let calc i =
        if i > getDimension sc then []
        else let rest = calc $ i + 1 in
          runEval $ do
            r       <- rpar rest
            current <- rpar $ calculateNthHomologyPar i sc
            rseq current
            rseq r
            return $ current:r in
  calc 0
--}