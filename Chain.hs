module Chain where

import Util
import Matrix
import Data.List

data Chain a = Chain [[a]] [a] Int a

getBasis (Chain simplices _ _ _) = map (map fromIntegral) simplices
getCoeffs (Chain _ coeffs _ _) = coeffs
getDim (Chain _ _ dim _) = dim
getOrder (Chain _ _ _ order) = order

verifyChainDimension :: Integral a => Chain a -> Bool
verifyChainDimension chain = let dim = getDim chain in
  forall (\a -> a == dim) (map (\l -> (fromIntegral (length l))) (getBasis chain))

--gets the boundary of a simplex, simplices and coeffs (3rd and 4th args) are part of the result,
--index is the current index of the proces in the simplex
getSimplexBoundary :: Integral a => Int -> [[a]] -> [a] -> Int -> a -> [a] -> Chain a
getSimplexBoundary max simplices coeffs index order simplex =
  if index == max then Chain simplices coeffs max order
  else let c = if simplex !! index `mod` 2 == 0 then 1
               else
                 if order == 2 then 0
                 else order - 1
           s = (take index simplex) ++ (drop (index + 1) simplex) in
    getSimplexBoundary max (s : simplices) (c : coeffs) (index + 1) order simplex

--finds the coefficient of a simplex in a chain
findCoeff :: Integral a => [a] -> Chain a -> a
findCoeff simplex chain =
  case elemIndex simplex (getBasis chain) of 
    Nothing -> 0
    Just i  -> (getCoeffs chain) !! i

--finds the sum of coefficients of a simplex in a list of chains
findAndSumCoeffs :: Integral a => [Chain a] -> [a] -> a
findAndSumCoeffs chains simplex = sum $ map (findCoeff simplex) chains

--combines like terms
--probably needs to be optimized
regroupChains :: Integral a => [Chain a] -> Chain a
regroupChains chains =
  let allSimplices = collect $ map getBasis chains
      coefficients = map (findAndSumCoeffs chains) allSimplices
      fstChain     = head chains in
  Chain allSimplices coefficients (getDim fstChain) (Chain.getOrder fstChain)

getActualCoeffs :: Integral a => [[a]] -> Chain a -> [a]
getActualCoeffs allSimplices chain =
  let coeffs    = getCoeffs chain 
      simplices = getBasis chain in
  map (\s -> let x = elemIndex s simplices in
             case x of
               Nothing -> 0
               Just i  -> coeffs !! i) allSimplices