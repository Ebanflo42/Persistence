module SimplicialComplex where

import Util
import Matrix
--import Chain
import Data.List
import Control.Parallel

data SimplicialComplex a = SimplicialComplex [[([a], [Int])]] [Matrix a] a

getSimplices (SimplicialComplex simplices _) = simplices
getDimension (SimplicialComplex simplices _) = length simplices
getOrder (SimplicialComplex _ order) = order

biggestSimplices :: Integral a => SimplicialComplex a -> [([a], [Int])]
biggestSimplices (SimplicialComplex simplices _) = last simplices

nDimensionalSimplices :: Integral a => Int -> SimplicialComplex a -> [([a], [Int])]
nDimensionalSimplices n sc = (getSimplices sc) !! n
  
makeNbrhoodGraph :: Ord a => a -> (b -> b -> a) -> [b] -> [[([Int], [Int])]]
makeNbrhoodGraph scale metric list =
  let helper _ []     = []
      helper i (x:xs) =
        let helper2 _ []     = []
            helper2 j (y:ys) =
              if metric x y < scale then ([i, j], []) : (helper2 (j + 1) ys)
              else (helper2 (j + 1) ys) in
        helper2 (i + 1) xs in
  (map (\n -> ([n], [])) [0..length list - 1]) : [helper 0 list]

checkAdjacentSimplices :: Int -> ([Int], Int) -> [([Int], Int)] -> [Maybe Int] -> [([Int], [Int])] -> [([Int], [Int])]
checkAdjacentSimplices dim simplex simplices adjacency result =
  case adjacency of
    []               -> result
    (Nothing:rest)   ->
      checkAdjacentSimplices dim simplex simplices rest result
    ((Just x):rest)  ->
      let commonSimplices = filter (\s -> exists x (fst s)) simplices
          len             = length commonSimplices in
      if length commonSimplices == dim then
        checkAdjacentSimplices dim simplex simplices rest ((x:(fst simplex), (snd simplex):(map snd simplices)):result)
      else if len < dim then
        checkAdjacentSimplices dim simplex simplices rest result
      else error "Neighborhood graph was a multigraph."

findHigherSimplices :: Int -> [([Int], Int)] -> [([Int], [Int])]
findHigherSimplices dim simplices =
  case simplices of
    []     -> []
    (x:xs) ->
      (checkAdjacentSimplices dim x xs (map (diffByOneElem $ fst x) $ map fst xs) []) ++ (findHigherSimplices dim xs)

constructSimplices :: Int -> [[([Int], [Int])]] -> [[([Int], [Int])]]
constructSimplices dim result =
  let currentSimplices = last result in
  case currentSimplices of
    [] -> init result
    _  ->
      constructSimplices (dim + 1) (result ++ [findHigherSimplices dim (mapWithIndex (\i e -> (e, i)) $ map fst currentSimplices)])

--may need to start dimension higher or lower, line 75 first arg of constructSimplices
makeVRComplex :: Ord a => a -> (b -> b -> a) -> [b] -> Int -> SimplicialComplex Int
makeVRComplex scale metric list =
  SimplicialComplex (constructSimplices 2 (makeNbrhoodGraph scale metric list))
--}

getEdgeBoundary :: Integral a => SimplicialComplex a -> Matrix a
getEdgeBoundary (SimplicialComplex simplices order) =
  let makeCoeff n = if order == 0 then minusOnePow n 
                    else (order - n) `mod` order in
  initializeMatrix order (map (\e -> [makeCoeff $ last $ fst e, makeCoeff $ head $ fst e]) $ simplices !! 1)

getSimplexBoundary :: Integral a => Int -> SimplicialComplex a -> ([a], [Int]) -> [a]
getSimplexBoundary dim (SimplicialComplex simplices ord) (simplex, indices) =
  let subsimplices = map (\index -> fst $ simplices !! (dim - 1) !! index) indices
      makeCoeff s  =
        let missing = findMissing s simplex in
        if ord == 0 then minusOnePow missing
        else (ord - missing) `mod` ord in
  map makeCoeff subsimplices

getBoundaryOperator :: Integral a => Int -> SimplicialComplex a -> Matrix a
getBoundaryOperator dim sc =
  initializeMatrix
    (SimplicialComplex.getOrder sc)
      (map (SimplicialComplex.getSimplexBoundary dim sc) $ (getSimplices sc) !! dim)

makeBoundaryOperators :: Integral a => SimplicialComplex a -> SimplicialComplex a
makeBoundaryOperators sc =
  let dim    = getDimension sc
      calc i =
        if i > dim then []
        else if i == 1 then (getEdgeBoundary sc) : (calc 2)
        else (getBoundaryOperator i sc) : (calc (i + 1)) in
  SimplicialComplex (getSimplices sc) (calc 1) (getOrder sc)
  