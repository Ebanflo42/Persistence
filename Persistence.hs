module Persistence where

import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

import Util
import Matrix
import MaximalCliques
import SimplicialComplex

--the index of the filtration where the simplex enters
--indices of the vertices in the original data set
--and indices of the faces in the filtration
data Simplex = Simplex Int (Vector Int) (Vector Int) deriving Show

getIndex (Simplex i _ _) = i
getVerts (Simplex _ v _) = v
getFaces (Simplex _ _ f) = f

sim2String :: Simplex -> String
sim2String (Simplex index vertices faces) =
  "Filtration index: " L.++ (show index) L.++
    "; Vertex indices: " L.++ (show vertices) L.++
      "; Boundary indices: " L.++ (show faces) L.++ "\n"

--number of vertices paired with
--2D array of simplices organized according to dimension
--each array of simplices should be sorted based on filtration index
type Filtration = (Int, Vector (Vector Simplex))

filtr2String :: Filtration -> String
filtr2String = (intercalate "\n") . toList . (V.map (L.concat . toList . (V.map sim2String))) . snd

--scales must be in decreasing order
makeFiltration :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeFiltration scales metric dataSet =
  let edgeInSimplex edge simplex = (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      (verts, simplices)         = makeVRComplex (L.head scales) metric dataSet
      graph                      =
        V.map (\v -> ((v ! 0), (v ! 1), metric (dataSet !! (v ! 0)) (dataSet !! (v ! 1)))) $ V.map fst $ simplices !! 0
      edgeTooLong scale edge     = scale < getEdgeLength (Graph graph) (edge ! 0) (edge ! 1)
      maxIndex                   = (L.length scales) - 1

      calcIndices 0 [] sc         = sc
      calcIndices i (scl:scls) sc =
        let longEdges = V.filter (edgeTooLong scl) $ V.map (\(Simplex i v f) -> v) $ V.head sc --find edges excluded by this scale
        in calcIndices (i - 1) scls $ V.map (V.map (\(Simplex j v f) ->
          if j == 0 then --if the simplex has not yet been assigned a fitration index
            if existsVec (\edge -> edgeInSimplex edge v) longEdges then Simplex i v f --if a long edge is in the simplex, assign it the current index
            else Simplex 0 v f --otherwise wait until next iteration
          else Simplex j v f)) sc --otherwise leave it alone

      sortFiltration simplices =
        let sortedSimplices =
              V.map (quicksort (\(Simplex i _ _, _) (Simplex j _ _, _) -> i < j)) $
                V.map (mapWithIndex (\i s -> (s, i))) simplices
            newFaces dim (Simplex i v f) =
              let findNew j =
                    case V.findIndex (\x -> snd x == j) $ sortedSimplices ! (dim - 1) of
                      Just k  -> k
                      Nothing -> error "Persistence.sortFiltration.newFaces.findNew"
              in Simplex i v (V.map findNew f)
        in
          if V.null simplices then simplices
          else mapWithIndex (\i ss -> V.map ((newFaces i) . fst) ss) sortedSimplices

  in (verts, sortFiltration $ --sort the simplices by filtration index
      calcIndices maxIndex (L.tail scales) $
        V.map (V.map (\(v, f) -> Simplex 0 v f)) $ fromList $ simplices)

edgeBoundaryBool :: Filtration -> BPolyMat
edgeBoundaryBool (verts, simplices) =
  if V.null simplices then V.empty
  else
    let makeBoundary (Simplex i v f) =
          replaceElem (v ! 0) (Power i) $ replaceElem (v ! 1) (Power i) $ V.replicate verts Zero
    in transposeMat $ V.map makeBoundary $ V.head simplices

boundOpBool :: Int -> Filtration -> BPolyMat
boundOpBool dim (verts, simplices) =
  let makeBoundary (Simplex i v f) =
        let makeMonomial j =
              if V.elem j f then
                let (Simplex k v f) = simplices ! (dim - 2) ! j
                in Power $ i - k
              else Zero
        in V.map makeMonomial $ 0 `range` ((V.length $ simplices ! (dim - 2)) - 1)
  in transposeMat $ V.map makeBoundary $ simplices ! (dim - 1)

boundaryOperatorsBool :: Filtration -> Vector BPolyMat
boundaryOperatorsBool f =
  let calc 1 = (edgeBoundaryBool f) `cons` V.empty
      calc i = (boundOpBool i f) `cons` (calc $ i - 1)
  in V.reverse $ calc $ V.length $ snd f

type BarCode = (Int, Maybe Int)

persistentHomology :: Filtration -> [[BarCode]]
persistentHomology filtration =
  let boundOps = boundaryOperatorsBool filtration
      max      = (V.length boundOps) - 1

      --return the original row indices of all the rows left in each matrix paired with the reduced form of every matrix
      reduce i ixs ops =
        if i == max then (ixs, (V.init ops) `snoc` (eschelonFormBool $ V.last ops))
        else
          let i1     = i + 1
              triple = eschelonAndNextBool (ops ! i) (ops ! i1)
          in reduce i1 (ixs `snoc` (two triple)) $ replaceElem i (one triple) $ replaceElem i1 (thr triple) ops

      getEdgeBarCodes op =
        let find row =
              case V.findIndex (\x -> x /= Zero) row of
                Just j  ->
                  case row ! j of
                    Power p -> (0, Just p)
                    Zero    -> error "The impossible happened; Persistence.persistentHomologyBool.getEdgeBarCodes.find"
                Nothing -> (0, Nothing)
        in L.filter (\(_, x) -> x /= Just 0) $ V.toList $ V.map find op

      getBarCodes i ops indices =
        if V.null ops then []
        else
          let op       = V.head ops
              maxIndex = V.length op - 1
              find j   = --find the pivot of the jth row and identify contribution to the homology group
                let rowIndex = getIndex $ (snd filtration) ! i ! (indices ! i ! j)
                in case V.findIndex (\x -> x /= Zero) $ op ! j of
                  Just k  ->
                    case op ! j ! k of
                      Power p ->
                        if j == maxIndex then (rowIndex, Just $ rowIndex + p):[]
                        else if p == 0 then find $ j + 1
                        else (rowIndex, Just $ rowIndex + p):(find $ j + 1)
                      Zero    -> error "The impossible happened; Persistence.persistentHomologyBool.getBarCodes.find"
                  Nothing ->
                    if j == maxIndex then (rowIndex, Nothing):[]
                    else (rowIndex, Nothing):(find $ j + 1)
          in (find 0):(getBarCodes (i + 1) (V.tail ops) indices)

      reduced = reduce 0 V.empty boundOps

  in (getEdgeBarCodes $ V.head $ snd reduced):(getBarCodes 0 (V.tail $ snd reduced) (fst reduced))