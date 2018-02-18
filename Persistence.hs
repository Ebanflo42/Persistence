module Persistence where

import Util
import Matrix
import Data.List as L
import MaximalCliques
import Data.Vector as V
import SimplicialComplex

--the index of the filtration where the simplex enters
--indices of the vertices in the original data set
--and indices of the faces in the filtration
data Simplex = Simplex Int (Vector Int) (Vector Int)

--2D array of simplices organized according to dimension
type Filtration = Vector (Vector Simplex)

instance Eq Simplex where
  (==) = \(Simplex a _ _) (Simplex b _ _) -> a == b

instance Ord Simplex where
  (>)   = \(Simplex a _ _) (Simplex b _ _) -> a > b
  (<)   = \(Simplex a _ _) (Simplex b _ _) -> a < b
  (<=)  = \(Simplex a _ _) (Simplex b _ _) -> a < b || a == b
  (>=)  = \(Simplex a _ _) (Simplex b _ _) -> a > b || a == b

sim2String :: Simplex -> String
sim2String (Simplex index vertices faces) =
  "Filtration index: " L.++ (show index) L.++
    "\nVertex indices: " L.++ (show vertices) L.++
      "\nBoundary indices: " L.++ (show faces) L.++ "\nend\n"

filtr2String :: Filtration -> String
filtr2String = (intercalate "\n") . toList . (V.map (L.concat . toList . (V.map sim2String)))

--scales must be in decreasing order
makeFiltration :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeFiltration scales metric dataSet =
  let edgeNotInSimplex edge       = forallVec (\x -> V.head edge /= x && V.last edge /= x)
      edgeToLong scale edge       = metric (dataSet !! (V.head edge)) (dataSet !! (V.last edge)) > scale
      calcIndices 0 [] sc         = sc
      calcIndices i (scl:scls) sc =
        let longEdges = V.filter (edgeToLong scl) $ V.map (\(Simplex i v f) -> v) $ V.head sc --find edges excluded by this scale
        in calcIndices (i - 1) scls $
          V.map (V.map (\(Simplex j v f) ->
            if j == 0 then --if the simplex has not yet been assigned a filtration index
              if forallVec (\edge -> edgeNotInSimplex edge v) longEdges then Simplex 0 v f --check if it isnt excluded by this scale
              else Simplex i v f --if it is excluded, assign to it the current filtration index
            else Simplex j v f)) sc --if it already has an index, do not change it
      maxIndex = (L.length scales) - 1
  in V.map quicksort $ --sort the simplices by filtration index
      calcIndices maxIndex (L.tail scales) $
        V.map (V.map (\(v, f) -> Simplex 0 v f)) $ fromList $ snd $
          makeVRComplex (L.head scales) metric dataSet

boundOpBool :: Int -> Filtration -> BPolyMat
boundOpBool dim filtration =
  let makeBoundary :: Simplex -> Vector BPolynomial
      makeBoundary (Simplex i v f) =
        V.map (\(Simplex k v f) -> (i - k)) $ V.map (\j -> filtration ! (dim - 1) ! j) f
  in transposeMat $ V.map makeBoundary $ filtration ! dim