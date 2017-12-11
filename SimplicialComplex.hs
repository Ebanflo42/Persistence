module SimplicialComplex
  ( SimplicialComplex
  , getDimension
  , makeNbrhdGraph
  , makeVRComplex
  , makeVRComplexFromGraph
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
type SimplicialComplex = [[([Int], [Int])]]

getDimension :: SimplicialComplex -> Int
getDimension simplices = length simplices - 1

--given a scale, a metric, and a data set this constructs a graph out of the points in the data set
--two points share an edge iff the distance between them is less than the scale
makeNbrhdGraph :: Ord a => a -> (b -> b -> a) -> [b] -> SimplicialComplex
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
constructSimplices :: Int -> SimplicialComplex -> SimplicialComplex
constructSimplices dim result =
  let currentSimplices = last result in
  case currentSimplices of
    [] -> init result --if there are no higher simplices to be found, return
    _  ->
      constructSimplices (dim + 1) (result ++ [findHigherSimplices dim (mapWithIndex (\i e -> (e, i)) $ map fst currentSimplices)])

--makes the Vietoris-Rips complex given a scale, metric, and data set
--may need to start dimension higher or lower, first arg of constructSimplices
makeVRComplex :: Ord a => a -> (b -> b -> a) -> [b] -> SimplicialComplex
makeVRComplex scale metric list =
  constructSimplices 2 $ makeNbrhdGraph scale metric list

makeVRComplexFromGraph :: SimplicialComplex -> SimplicialComplex
makeVRComplexFromGraph = constructSimplices 2

--gets the first boundary operator (because edges don't need to point to their subsimplices)
makeEdgeBoundary :: Bool -> SimplicialComplex -> Matrix Int
makeEdgeBoundary ismod2 simplices =
  let makeCoeff = \n -> if ismod2 || n `mod` 2 == 0 then 1 else -1 in
  initializeMatrix ismod2 $ transpose $
    map (\edge ->
      map (\vert -> let v = head vert in
                    if v == head edge || v == last edge then makeCoeff v
                    else 0) $ map fst $ head simplices) $ map fst (simplices !! 1)

--gets the boundary coefficients for a simplex of dimension 2 or greater
makeSimplexBoundary :: Bool -> Int -> SimplicialComplex -> ([Int], [Int]) -> [Int]
makeSimplexBoundary ismod2 dim simplices (simplex, indices) =
  let makeCoeff s  =
        if ismod2 then 1
        else if (findMissing s simplex) `mod` 2 == 0 then 1
        else -1 in
  mapWithIndex (\i s -> if exists i indices then makeCoeff s else 0) (map fst $ simplices !! (dim - 1))

--makes boundary operator for all simplices of dimension 2 or greater
makeBoundaryOperator :: Bool -> Int -> SimplicialComplex -> Matrix Int
makeBoundaryOperator ismod2 dim sc =
  initializeMatrix ismod2 $ transpose $
    map (makeSimplexBoundary ismod2 dim sc) $ sc !! dim

--makes all the boundary operators, should always be called before calculating homology
makeBoundaryOperators :: Bool -> SimplicialComplex -> [Matrix Int]
makeBoundaryOperators ismod2 sc =
  let dim    = getDimension sc
      calc i =
        if i > dim then []
        else if i == 1 then (makeEdgeBoundary ismod2 sc) : (calc 2)
        else (makeBoundaryOperator ismod2 i sc) : (calc (i + 1)) in
 calc 1

calculateNthHomology :: Bool -> Int -> SimplicialComplex -> [Int]
calculateNthHomology ismod2 n sc =
  if n == 0 then (getUnsignedDiagonal . getSmithNormalForm . (makeEdgeBoundary ismod2)) sc
  else
    let dim = getDimension sc
        boundOps =
          case n of
            x | x == dim -> (Nothing, Just $ makeBoundaryOperator ismod2 n sc)
            x | x > dim  -> (Nothing, Nothing)
            _            -> (Just $ makeBoundaryOperator ismod2 (n + 1) sc, Just $ makeBoundaryOperator ismod2 n sc) in
    case boundOps of
      (Nothing, Nothing) -> []
      (Nothing, Just mx) -> replicate (length $ getElems $ findKernel mx) 0
      (Just m1, Just m2) -> (getUnsignedDiagonal . getSmithNormalForm . (multiply $ findKernel m2)) m1

calculateNthHomologyPar :: Bool -> Int -> SimplicialComplex -> [Int]
calculateNthHomologyPar ismod2 n sc =
  if n == 0 then (getUnsignedDiagonal . getSmithNormalFormPar . (makeEdgeBoundary ismod2)) sc
  else
    let dim = getDimension sc
        boundOps =
          case n of
            x | x == dim -> (Nothing, Just $ makeBoundaryOperator ismod2 n sc)
            x | x > dim  -> (Nothing, Nothing)
            _            -> (Just $ makeBoundaryOperator ismod2 (n + 1) sc, Just $ makeBoundaryOperator ismod2 n sc) in
    case boundOps of
      (Nothing, Nothing) -> []
      (Nothing, Just mx) -> replicate (length $ getElems $ findKernel mx) 0
      (Just m1, Just m2) -> (getUnsignedDiagonal . getSmithNormalFormPar . (multiply $ findKernel m2)) m1

calculateHomology :: Bool -> SimplicialComplex -> [[Int]]
calculateHomology ismod2 sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperators ismod2 sc
      calc i
        | i == 0    = (getUnsignedDiagonal $ getSmithNormalFormPar $ head boundOps) : (calc 1)
        | i == dim  = [replicate (length $ getElems $ findKernel $ boundOps !! i) 0]
        | otherwise =
          ((getUnsignedDiagonal . getSmithNormalFormPar . (multiply $ findKernel (boundOps !! i))) (boundOps !! (i + 1))) : (calc (i + 1)) in
  calc 0        

calculateHomologyPar :: Bool -> SimplicialComplex -> [[Int]]
calculateHomologyPar ismod2 sc =
  let dim      = getDimension sc
      boundOps = makeBoundaryOperators ismod2 sc
      calc i
        | i == 0    =
          let rest = calc 1
              current = getUnsignedDiagonal $ getSmithNormalFormPar $ head boundOps in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r)
        | i == dim  = [replicate (length $ getElems $ findKernel $ boundOps !! i) 0]
        | otherwise =
          let rest = calc $ i + 1
              current = (getUnsignedDiagonal . getSmithNormalFormPar . (multiply $ findKernel (boundOps !! i))) (boundOps !! (i + 1)) in
          runEval $ do
            c <- rpar current
            r <- rpar rest
            return (c:r) in
  calc 0