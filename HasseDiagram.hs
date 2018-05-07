module HasseDiagram where

{--OVERVIEW---------------------------------------------------------------

This module implements algorithms for admissible Hasse diagrams. A Hasse diagram is admissible if it is stratified and oriented. A diagram is stratified if all the vertices can be arranged in rows such that all the sources of each vertex are in the next highest row and all the targets are in the next lowest row. A diagram is oriented if every vertex has a linear ordering on its targets.

A node in the diagram is represented as a tuple: the indices of the level 0 nodes in the diagram that are reachable from this node, the indices of targets in the next lowest level, and the indices of the sources in the next highest level. The entire diagram is simply an array where each entry is an array representing a particular level; index 0 represents level 0, etc.

Any directed graph can be encoded as an admissible Hasse diagram with 2 levels. The edges are level 1 and the vertices are level 0. The ordering on the targets of a node representing an edge is simply the initial vertex first and the final vertex second.

Any oriented simplicial complex can also be encoded as an admissible Hasse diagram. A node is a simplex, the targets are the faces of the simplex, and the sources are simplices of which this simplex is a face.

The main feature of this module is an algorithm which takes the Hasse diagram of a directed graph and generates the Hasse diagram of the directed flag complex - the simplicial complex whose simplices are acyclic directed subgraphs of the given graph.

--}

import Util
import SimplicialComplex

import Data.Vector as V

--vertices, faces, parents
type Node = (Vector Int, Vector Int, Vector Int)

type HasseDiagram = Vector (Vector Node)

encodeGraph :: Int -> [(Int, Int)] -> HasseDiagram
encodeGraph numVerts connections =
  let verts = V.map (\n -> (n `cons` V.empty, V.empty, V.empty)) $ 0 `range` (numVerts - 1)

      encodeEdges _ vertices edges []          = V.empty `snoc` vertices `snoc` edges
      encodeEdges n vertices edges ((i, j):xs) =
        let v1 = vertices ! i; v2 = vertices ! j; edge = V.empty `snoc` i `snoc` j
        in encodeEdges (n + 1)
          (replaceElem i (one v1, two v1, (thr v1) `snoc` n) $
            replaceElem j (one v2, two v2, (thr v2) `snoc` n) vertices) 
              (edges `snoc` (edge, edge, V.empty)) xs

  in encodeEdges 0 verts V.empty connections

directedFlagComplex :: HasseDiagram -> HasseDiagram
directedFlagComplex directedGraph =
  let edges    = V.last directedGraph
      fstSinks :: Vector (Vector Int)
      fstSinks =
        V.map (\e -> 
          V.map (\(e0, _) -> (two e0) ! 1) $
            findBothElems (\e1 e2 -> (two e1) ! 1 == (two e2) ! 1)
              (V.filter (\e0 -> (two e0) ! 0 == (two e) ! 0) edges) (V.filter (\e0 -> (two e0) ! 0 == (two e) ! 1) edges)) edges

      --take last level of nodes and their sinks. return modified lat level, new level, and new sinks
      makeLevel :: HasseDiagram -> Vector Node -> Vector (Vector Int) -> (Vector Node, Vector Node, Vector (Vector Int))
      makeLevel result oldNodes oldSinks =
        let maxindex = V.length oldNodes

            --given a node and a specific sink, construct a new node with new sinks that has the given index. Fst output is the modified input nodes, snd output is the new node, thrd output is the sinks of the new node
            makeNode :: Int -> Int -> Int -> (Node, Vector Int) -> Vector Node -> (Vector Node, Node, Vector Int)
            makeNode newIndex oldIndex sink (node, sinks) nodes =
              let verts    = sink `cons` (one node) --the vertices of the new simplex are the vertices of the old simplex plus the sink
                  numFaces = V.length $ two node

                  --find all the faces of the new node by looking at the faces of the old node
                  testTargets i oldNode onodes newNode newSinks =
                      let currentFace   = (V.last result) ! ((two oldNode) ! i)
                          possibleFaces = V.map (\j -> (j, onodes ! j)) $ thr currentFace
                      in
                        if i == numFaces then (onodes, newNode, newSinks)
                        else
                      case V.find (\(_, (v, _, _)) -> V.head v == sink) possibleFaces of
                        Just (j, n) ->
                          let face = onodes ! j
                          in testTargets (i + 1) oldNode (replaceElem j (one face, two face, newIndex `cons` (thr face)) onodes) (one newNode, j `cons` (two newNode), thr newNode) (newSinks |^| (oldSinks ! j))
                        Nothing     -> error "Something went wrong, HasseDiagram.directedFlagComplex.makeDiagram.makeNode.testTargets"

              in testTargets 0 node nodes (verts, oldIndex `cons` V.empty, V.empty) sinks

            loopSinks :: Int -> Int -> Vector Node -> (Vector Node, Vector Node, Vector (Vector Int), Int)
            loopSinks nodeIndex lastIndex nodes =
              let node     = oldNodes ! nodeIndex
                  sinks    = oldSinks ! nodeIndex
                  numSinks = V.length sinks

                  loop i (modifiedNodes, newNodes, newSinks) =
                    if i == numSinks then (modifiedNodes, newNodes, newSinks, i)
                    else
                      let (modNodes, newNode, ns) = makeNode (lastIndex + i) nodeIndex (sinks ! i) (node, sinks) modifiedNodes
                      in loop (i + 1) (modNodes, newNodes `snoc` newNode, newSinks `snoc` ns)

              in loop 0 (nodes, V.empty, V.empty)

            loopNodes :: Int -> Int -> Vector Node -> Vector Node -> Vector (Vector Int) -> (Vector Node, Vector Node, Vector (Vector Int))
            loopNodes i n nodes newNodes newSinks =
              if i == maxindex then (nodes, newNodes, newSinks)
              else
                let (modifiedNodes, nnodes, nsinks, lastIndex) = loopSinks i n nodes
                in loopNodes (i + 1) lastIndex modifiedNodes (newNodes V.++ nnodes) (newSinks V.++ nsinks)

        in loopNodes 0 0 oldNodes V.empty V.empty

      loopLevels :: HasseDiagram -> Vector Node -> Vector (Vector Int) -> HasseDiagram
      loopLevels diagram nextNodes sinks =
        let (modifiedNodes, newNodes, newSinks) = makeLevel diagram nextNodes sinks
            newDiagram                          = diagram `snoc` modifiedNodes
        in
          if V.null newNodes then newDiagram
          else loopLevels newDiagram newNodes newSinks

  in loopLevels directedGraph edges fstSinks

toSimplicialComplex :: HasseDiagram -> SimplicialComplex
toSimplicialComplex diagram = (V.length $ V.head diagram, V.map (V.map not3) $ V.tail diagram)
