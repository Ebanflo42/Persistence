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