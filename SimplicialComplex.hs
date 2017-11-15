module SimplicialComplex where

import Util
import Matrix
import Chain
import Data.List

data SimplicialComplex a = SimplicialComplex [[a]] [[a]] (Maybe a) a

getParents (SimplicialComplex parents _ _ _) = parents
getChildren (SimplicialComplex _ children _ _) = children
getDimension (SimplicialComplex parents _ dim _) =
  case dim of
    Nothing ->
      fromIntegral $ foldl1 max (map length parents)
    Just d  -> d
getOrder (SimplicialComplex _ _ _ order) = order

biggestSimplices :: Integral a => SimplicialComplex a -> [[a]]
biggestSimplices sc =
  let dim = getDimension sc in
  filter (\s -> (fromIntegral . length) s == dim) (getParents sc)

nDimensionalSimplices :: Integral a => a -> SimplicialComplex a -> [[a]]
nDimensionalSimplices n sc = filter (\s -> (fromIntegral . length) s == n) (getParents sc ++ getChildren sc)

makeNeighborhoodGraph :: Ord a => a -> (b -> b -> a) -> [b] -> [(Int, Int)]
makeNeighborhoodGraph scale metric list =
  let helper _ []     = []
      helper i (x:xs) =
        let helper2 _ []     = []
            helper2 j (y:ys) =
              if metric x y < scale then (i, j) : (helper2 (j + 1) ys)
              else (helper2 (j + 1) ys) in
        helper2 (i + 1) xs in
  helper 0 list

levenshtein :: String -> String -> Int
levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]