module SimplicialComplex where

import Util
import Matrix
import Chain
import Data.List

data SimplicialComplex a = SimplicialComplex [[[a]]] a

getSimplices (SimplicialComplex simplices _) = simplices
getDimension (SimplicialComplex simplices _) = length simplices
getOrder (SimplicialComplex _ order) = order

biggestSimplices :: Integral a => SimplicialComplex a -> [[a]]
biggestSimplices (SimplicialComplex simplices _) = last simplices

nDimensionalSimplices :: Integral a => Int -> SimplicialComplex a -> [[a]]
nDimensionalSimplices n sc = (getSimplices sc) !! n

makeNbrhoodGraph :: Ord a => a -> (b -> b -> a) -> [b] -> [[[Int]]]
makeNbrhoodGraph scale metric list =
  let helper _ []     = []
      helper i (x:xs) =
        let helper2 _ []     = []
            helper2 j (y:ys) =
              if metric x y < scale then [i, j] : (helper2 (j + 1) ys)
              else (helper2 (j + 1) ys) in
        helper2 (i + 1) xs in
  (map (\n -> [n]) [0..length list - 1]) : [helper 0 list]

checkAdjacentSimplices :: Int -> [Int] -> [[Int]] -> [Maybe Int] -> [[Int]] -> [[Int]]
checkAdjacentSimplices dim simplex simplices adjacency result =
  case adjacency of
    []               -> result
    (Nothing:rest)   ->
      checkAdjacentSimplices dim simplex simplices rest result
    ((Just x):rest)  ->
      let commonSimplices = filter (exists x) simplices in
      if length commonSimplices == dim then
        checkAdjacentSimplices dim simplex simplices rest ((x:simplex):result)
      else
        checkAdjacentSimplices dim simplex simplices rest result

findHigherSimplices :: Int -> [[Int]] -> [[Int]]
findHigherSimplices dim simplices =
  case simplices of
    []     -> []
    (x:xs) ->
      (checkAdjacentSimplices dim x xs (map (diffByOneElem x) xs) []) ++ (findHigherSimplices dim xs)

constructSimplices :: Int -> [[[Int]]] -> [[[Int]]]
constructSimplices dim result =
  let currentSimplices = last result in
  case currentSimplices of
    [] -> init result
    _  ->
      constructSimplices (dim + 1) (result ++ [findHigherSimplices dim currentSimplices])
  {--
  case vertices of
    []     -> result
    (v:vs) ->
      constructSimplices vs edges $
        map (\simplex ->
          if forall (\point ->
            existsPredicate (\edge ->
              (fst edge == v && snd edge == point) || (fst edge == point && snd edge == v)) edges) simplex
                then v:simplex else simplex)
                  result
  --}
makeVRComplex :: Ord a => a -> (b -> b -> a) -> [b] -> Int -> SimplicialComplex Int
makeVRComplex scale metric list =
  SimplicialComplex (constructSimplices 2 ((map (\n -> [n]) [0..length list - 1]) : (makeNbrhoodGraph scale metric list)))
--}
  

levenshtein :: String -> String -> Int
levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]