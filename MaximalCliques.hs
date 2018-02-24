{- |
Module      :  Data.Algorithm.MaximalCliques
Copyright   :  (c) Gershom Bazerman, 2010
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

This library uses the Bron-Kerbosch algorithm to enumerate all maximal cliques in an undirected graph. A clique is a set of nodes such that there is an edge between every node and every other node in the set. A maximal clique is a clique such that no node may be added while preserving the clique property.

Maximal clique enumeration is ExpTime complete, and even finding the greatest single clique (the maximum clique) is NP-complete. The Bron-Kerbosch algorithm is known to run well in practice while maintaining a simple implementation. If more efficiency is desired, there are now better algorithms. See, for example, Makino and Uno: <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.138.705>. Patches providing improved implementations are more than welcome.

-}
module MaximalCliques where

import Data.List (mapAccumL)
import qualified Data.IntSet as S
import qualified Data.Vector as V

-- | Given a list of nodes, and a function that determines whether there is an edge between any two nodes, yields a list of maximal cliques -- sets of nodes such that every node is connected to every other, and such that no other node may be added while maintaining this property.
getMaximalCliques :: (a -> a -> Bool) -> [a] -> [[a]]
getMaximalCliques tolFun xs = map (map (fst . (V.!) lv) . S.toList) $
                              maximalCliques pickpivot (snd . ((V.!) lv)) (S.fromList $ map fst lnodes)
    where lnodes = zip [0..] xs
          lnodes' = map (\(k,n) -> (n,S.fromList $ filter (/=k) $ map fst $ filter (tolFun n . snd) lnodes)) lnodes
          lv = V.fromList lnodes'
          pickpivot p x = head $ S.elems p ++ S.elems x
          
-- | The Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph.
-- <http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>. Works on nodes represented as 'Int's.
maximalCliques :: (S.IntSet -> S.IntSet -> Int) -- ^ A function that given two 'IntSet's, chooses a member of one as a pivot.
               -> (Int -> S.IntSet)  -- ^ A function that given a node id, yields the set of its neighbors.
               -> S.IntSet -- ^ The set of all nodes in the graph.
               -> [S.IntSet] -- ^ An enumeration of all maximal cliques in the graph.
maximalCliques pickpivot neighborsOf nodeset = go S.empty nodeset S.empty
    where go r p x
              | S.null p && S.null x = [r]
              | otherwise =
                  let pivot = pickpivot p x
                      step' (p',x') v =
                          let nv  = neighborsOf v
                          in ((S.delete v p', S.insert v x'), go (S.insert v r) (S.intersection nv p') (S.intersection nv x'))
                  in concat . snd $ mapAccumL step' (p,x) $ S.elems (p S.\\ neighborsOf pivot)