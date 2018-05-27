{- |
Module     : Persistence.HasseDiagram
Copyright  : (c) Eben Cowley, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module implements algorithms for admissible Hasse diagrams. A Hasse diagram is admissible if it is stratified and oriented. A diagram is stratified if all the vertices can be arranged in rows such that all the sources of each vertex are in the next highest row and all the targets are in the next lowest row. A diagram is oriented if every vertex has a linear ordering on its targets.

A node in the diagram is represented as a tuple: the indices of the level 0 nodes in the diagram that are reachable from this node, the indices of targets in the next lowest level, and the indices of the sources in the next highest level. The entire diagram is simply an array where each entry is an array representing a particular level; index 0 represents level 0, etc.

Any directed graph can be encoded as an admissible Hasse diagram with 2 levels. The edges are level 1 and the vertices are level 0. The ordering on the targets of a node representing an edge is simply the initial vertex first and the final vertex second.

Any oriented simplicial complex can also be encoded as an admissible Hasse diagram. A node is a simplex, the targets are the faces of the simplex, and the sources are simplices of which this simplex is a face.

The main feature of this module is an algorithm which takes the Hasse diagram of a directed graph and generates the Hasse diagram of the directed flag complex - the simplicial complex whose simplices are acyclic directed subgraphs of the given graph.

-}

module HasseDiagram
  ( Node
  , HasseDiagram
  , hsd2String
  , encodeDirectedGraph
  , directedFlagComplex
  , toSimplicialComplex
  ) where

import Util
import SimplicialComplex

import Data.List as L
import Data.Vector as V

--vertices, faces, parents
type Node = (Vector Int, Vector Int, Vector Int)

type HasseDiagram = Vector (Vector Node)

hsd2String :: HasseDiagram -> String
hsd2String = (L.intercalate "\n\n") . V.toList . (V.map (L.intercalate "\n" . V.toList . V.map show))

encodeDirectedGraph :: Int -> [(Int, Int)] -> HasseDiagram
encodeDirectedGraph numVerts cxns =
  let verts       = V.map (\n -> (n `cons` V.empty, V.empty, V.empty)) $ 0 `range` (numVerts - 1)

      encodeEdges _ vertices edges []          = V.empty `snoc` vertices `snoc` edges
      encodeEdges n vertices edges ((i, j):xs) =
        let v1 = vertices ! i; v2 = vertices ! j; edge = V.empty `snoc` j `snoc` i
        in encodeEdges (n + 1)
          (replaceElem i (one v1, two v1, (thr v1) `snoc` n) $
            replaceElem j (one v2, two v2, (thr v2) `snoc` n) vertices) 
              (edges `snoc` (edge, edge, V.empty)) xs

  in encodeEdges 0 verts V.empty cxns

directedFlagComplex :: HasseDiagram -> HasseDiagram
directedFlagComplex directedGraph =
  let edges    = V.last directedGraph
      fstSinks =
        V.map (\e ->
          V.map (\(e0, _) -> (two e0) ! 0) $
            findBothElems (\e1 e2 -> (two e1) ! 0 == (two e2) ! 0)
              (V.filter (\e0 -> (two e0) ! 1 == (two e) ! 1) edges) (V.filter (\e0 -> (two e0) ! 1 == (two e) ! 0) edges)) edges

      --take last level of nodes and their sinks. return modified last level, new level, and new sinks
      makeLevel :: Bool -> HasseDiagram -> Vector Node -> Vector (Vector Int) -> (Vector Node, Vector Node, Vector (Vector Int))
      makeLevel fstIter result oldNodes oldSinks =
        let maxindex = V.length oldNodes

            --given a node and a specific sink, construct a new node with new sinks that has the given index. Fst output is the modified input nodes, snd output is the new node, thrd output is the sinks of the new node
            makeNode :: Int -> Int -> Int -> Vector Node -> Vector Int -> (Vector Node, Node, Vector Int)
            makeNode newIndex oldIndex sinkIndex nodes sinks =
              let sink     = sinks ! sinkIndex
                  oldNode  = nodes ! oldIndex
                  verts    = sink `cons` (one oldNode) --the vertices of the new simplex are the vertices of the old simplex plus the sink
                  numFaces = V.length $ two oldNode

                  --find all the faces of the new node by looking at the faces of the old node
                  testTargets :: Int -> Node -> Vector Node -> Node -> Vector Int -> (Vector Node, Node, Vector Int)
                  testTargets i onode onodes newNode newSinks =
                    let faceVerts =
                          if fstIter then one $ (V.last $ V.init $ result) ! ((two onode) ! i)
                          else one $ (V.last $ result) ! ((two onode) ! i)
                    in
                      if i == numFaces then (onodes, newNode, newSinks)
                      else
                        case V.find (\(_, (v, _, _)) -> V.head v == sink && V.tail v == faceVerts) $ mapWithIndex (\j n -> (j, n)) onodes of
                          Just (j, n) ->
                            testTargets (i + 1) onode
                              (replaceElem j (one n, two n, (thr n) `smartSnoc` newIndex) onodes)
                                (one newNode, (two newNode) `snoc` j, thr newNode) (newSinks |^| (oldSinks ! j))
                          Nothing     -> error "Face not found, HasseDiagram.directedFlagComplex.makeDiagram.makeNode.testTargets"

              in testTargets 0 oldNode nodes (verts, oldIndex `cons` V.empty, V.empty) sinks

            loopSinks :: Int -> Int -> Vector Node -> (Vector Node, Vector Node, Vector (Vector Int), Int)
            loopSinks nodeIndex lastIndex nodes =
              let node     = oldNodes ! nodeIndex
                  sinks    = oldSinks ! nodeIndex
                  numSinks = V.length sinks

                  loop i (modifiedNodes, newNodes, newSinks) =
                    if i == numSinks then (modifiedNodes, newNodes, newSinks, i + lastIndex)
                    else
                      let (modNodes, newNode, ns) = makeNode (i + lastIndex) nodeIndex i modifiedNodes sinks
                      in loop (i + 1) (modNodes, newNodes `snoc` newNode, newSinks `snoc` ns)

              in loop 0 (nodes, V.empty, V.empty)

            loopNodes :: Int -> Int -> Vector Node -> Vector Node -> Vector (Vector Int) -> (Vector Node, Vector Node, Vector (Vector Int))
            loopNodes i lastIndex nodes newNodes newSinks =
              if i == maxindex then (nodes, newNodes, newSinks)
              else
                let (modifiedNodes, nnodes, nsinks, index) = loopSinks i lastIndex nodes
                in loopNodes (i + 1) lastIndex modifiedNodes (newNodes V.++ nnodes) (newSinks V.++ nsinks)

        in loopNodes 0 0 oldNodes V.empty V.empty

      loopLevels :: Int -> HasseDiagram -> Vector Node -> Vector (Vector Int) -> HasseDiagram
      loopLevels iter diagram nextNodes sinks =
        let (modifiedNodes, newNodes, newSinks) = makeLevel (iter < 2) diagram nextNodes sinks
            newDiagram                          = diagram `snoc` modifiedNodes
        in
          if V.null newNodes then newDiagram
          else loopLevels (iter + 1) newDiagram newNodes newSinks

  in loopLevels 0 directedGraph edges fstSinks

toSimplicialComplex :: HasseDiagram -> SimplicialComplex
toSimplicialComplex diagram =
  let sc = V.map (V.map not3) $ V.tail diagram
  in (V.length $ V.head diagram, (V.map (\(v, _) -> (v, V.empty)) $ sc ! 0) `cons` V.tail sc)
