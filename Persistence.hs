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
makeFiltrationFast :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeFiltrationFast scales metric dataSet =
  let edgeInSimplex edge simplex  = (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      ((verts, simplices), graph) = makeVRComplexFast (L.head scales) metric dataSet
      edgeTooLong scale edge      = scale <= (fst $ graph ! (edge ! 0) ! (edge ! 1))
      maxIndex                    = (L.length scales) - 1

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

--scales must be in decreasing order
makeFiltrationLight :: (Ord a, Eq b) => [a] -> (b -> b -> a) -> [b] -> Filtration
makeFiltrationLight scales metric dataSet =
  let edgeInSimplex edge simplex  = (existsVec (\x -> V.head edge == x) simplex) && (existsVec (\x -> V.last edge == x) simplex)
      (verts, simplices)          = makeVRComplexLight (L.head scales) metric dataSet
      vector                      = V.fromList dataSet
      edgeTooLong scale edge      = scale <= (metric (vector ! (edge ! 0)) (vector ! (edge ! 1)))
      maxIndex                    = (L.length scales) - 1

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

edgeBoundary :: Filtration -> BPolyMat
edgeBoundary (verts, simplices) =
  if V.null simplices then V.empty
  else
    let makeBoundary (Simplex i v f) =
          replaceElem (v ! 0) (Power i) $ replaceElem (v ! 1) (Power i) $ V.replicate verts Zero
    in transposeMat $ V.map makeBoundary $ V.head simplices

boundOp :: Int -> Filtration -> BPolyMat
boundOp dim (verts, simplices) =
  let makeBoundary (Simplex i v f) =
        let makeMonomial j =
              if V.elem j f then
                let (Simplex k v f) = simplices ! (dim - 2) ! j
                in Power $ i - k
              else Zero
        in V.map makeMonomial $ 0 `range` ((V.length $ simplices ! (dim - 2)) - 1)
  in transposeMat $ V.map makeBoundary $ simplices ! (dim - 1)

boundaryOperators :: Filtration -> Vector BPolyMat
boundaryOperators f =
  let calc 1 = (edgeBoundary f) `cons` V.empty
      calc i = (boundOp i f) `cons` (calc $ i - 1)
  in
    if L.null $ snd f then V.empty
    else V.reverse $ calc $ V.length $ snd f

type BarCode = (Int, Maybe Int)

persistentHomology :: Filtration -> [[BarCode]]
persistentHomology filtration =
  let boundOps = boundaryOperators filtration
      numOps   = V.length boundOps - 1

      --return the pivot degrees of every reduced column echelon form of every boundary operator
      reduce :: Int -> Vector (Vector Int) -> Vector BPolyMat -> (Vector (Vector Int), Vector BPolyMat)
      reduce i degs ops =
        if i == numOps then ((V.replicate (V.length $ ops ! 0) 0) `cons` degs, (V.init ops) `snoc` (echelonFormBool $ V.last ops))
        else
          let i1     = i + 1
              triple = echelonAndNextBool (ops ! i) (ops ! i1)
          in reduce i1 (degs `snoc` (two triple)) $ replaceElem i (one triple) $ replaceElem i1 (thr triple) ops

      findPivots :: BPolyMat -> Vector (Maybe Int)
      findPivots matrix =
        let rows = V.length matrix
            find i last =
              if i == rows then V.empty
              else
                case reverseFind (\x -> x /= Zero) $ matrix ! i of
                  Just (Power p, j) ->
                    if j > last then (Just p) `cons` (find (i + 1) j)
                    else Nothing `cons` (find (i + 1) last)
                  Nothing           -> Nothing `cons` (find (i + 1) last)
        in find 0 (-1)

      getBarCodes :: Vector (Vector Int) -> Vector BPolyMat -> [[BarCode]]
      getBarCodes degrees matrices =
        if V.null matrices then []
        else
          let m  = V.head matrices
              ps = findPivots m
              ds = V.head degrees
          in (V.toList $ V.map (\(Just x) -> x) $ V.filter (\x -> x /= Nothing) $
            mapWithIndex (\i row ->
              let d = ds ! i
              in case ps ! i of
                Just pow ->
                  if pow == 0 then Nothing
                  else Just (d, Just $ pow + d)
                Nothing  -> Just (d, Nothing)) m):(getBarCodes (V.tail indices) (V.tail matrices))

      (indices, reduced) = reduce 0 V.empty boundOps

  in
    if V.null boundOps then [L.replicate (fst filtration) (0, Nothing)]
    else getBarCodes indices reduced