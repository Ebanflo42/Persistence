module Persistence where

import Util
import Data.List as L
import MaximalCliques
import Data.Vector as V
import SimplicialComplex

--the index of the filtration where the simplex enters, indices of the vertices in the original data set
type Simplex = (Int, Vector Int, Vector Int)

--2D array of simplices organized according to dimension
type Filtration = Vector (Vector Simplex)

sim2String :: Simplex -> String
sim2String (index, vertices, faces) =
  "Filtration index: " L.++ (show index) L.++
    "\nVertex indices: " L.++ (show vertices) L.++
      "\nBoundary indices: " L.++ (show faces) L.++ "\n"

filtr2String :: Filtration -> String
filtr2String = (intercalate "\n") . toList . (V.map (L.concat . toList . (V.map sim2String)))

--scales must be in decreasing order
makeFiltration :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeFiltration scales metric dataSet =
  let calcIndices 0 [] sc         = sc
      calcIndices i (scl:scls) sc =
        calcIndices (i - 1) scls $
          V.map (V.map (\simplex ->
            if one simplex == 0 then
              let indices = two simplex
              in
                if forallRelation (\j k -> metric (dataSet !! j) (dataSet !! k) < scl) indices then simplex
                else (i + 1, indices, thr simplex)
            else simplex)) sc
  in calcIndices (L.length scales - 1) (L.tail scales) $
    V.map (V.map (\s -> (0, fst s, snd s))) $ fromList $ snd $
      makeVRComplex (L.head scales) metric dataSet