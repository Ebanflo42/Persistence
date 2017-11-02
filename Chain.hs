module Chain where

import Util
import Matrix
import Data.List

data Chain a = Chain [[a]] [a] a a

getSimplices (Chain simplices _ _ _) = map (map fromIntegral) simplices
getCoeffs (Chain _ coeffs _ _) = coeffs
getDim (Chain _ _ dim _) = dim
getOrder (Chain _ _ _ order) = order

verifyChainDimension :: Integral a => Chain a -> Bool
verifyChainDimension chain = let dim = getDim chain in
  forall (\a -> a == dim) (map (\l -> (fromIntegral (length l))) (getSimplices chain))

--gets the boundary of a simplex, simplices and coeffs (3rd and 4th args) are part of the result,
--index is the current index of the proces in the simplex
getSimplexBoundary :: Integral a => [[a]] -> [a] -> a -> a -> [a] -> Chain a
getSimplexBoundary simplices coeffs index order simplex =
  case simplex of
    []       -> Chain simplices coeffs (((fromIntegral . length . head) simplices) - 1) order
    (x : xs) ->
      let s = (tail simplex) ++ xs
          c = if index `mod` 2 == 0 then 1
              else
                if order == 2 then 0
                else order - 1 in
      getSimplexBoundary (s : simplices) (c : coeffs) (index + 1) order simplex

--finds the coefficient of a simplex in a chain
findCoeff :: Integral a => Chain a -> [a] -> a
findCoeff chain simplex =
  case elemIndex simplex (getSimplices chain) of 
    Nothing -> 0
    Just i  -> (getCoeffs chain) !! i

--finds the sum of coefficients of a simplex in a list of chains
findAndSumCoeffs :: Integral a => [Chain a] -> [a] -> a
findAndSumCoeffs chains simplex =
  case chains of
    []     -> 0
    (c:cs) -> findCoeff c simplex + findAndSumCoeffs cs simplex

--combines like terms
--probably needs to be optimized
regroupChains :: Integral a => [Chain a] -> Chain a
regroupChains chains =
  let allSimplices = collect $ map getSimplices chains
      coefficients = map (findAndSumCoeffs chains) allSimplices
      fstChain     = head chains in
  Chain allSimplices coefficients (getDim fstChain) (Chain.getOrder fstChain)

getActualCoeffs :: Integral a => [[a]] -> Chain a -> [a]
getActualCoeffs allSimplices chain =
  let coeffs    = getCoeffs chain 
      simplices = getSimplices chain in
  map (\s -> let x = elemIndex s simplices in
             case x of 
               Nothing -> 0
               Just i  -> coeffs !! i) allSimplices

getBoundaryOperator :: Integral a => [[a]] -> a -> Matrix a
getBoundaryOperator simplices order =
  let simplexBounds = map (getSimplexBoundary [] [] 0 order) simplices
      allSimplices  = collect $ map getSimplices simplexBounds in
  Matrix (map (getActualCoeffs allSimplices) simplexBounds) order
  