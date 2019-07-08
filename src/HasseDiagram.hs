{- |
Module     : Persistence.HasseDiagram
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module implements algorithms for admissible Hasse diagrams. A Hasse diagram is admissible if it is stratified and oriented. A diagram is stratified if all the vertices can be arranged in rows such that all the sources of each vertex are in the next highest row and all the targets are in the next lowest row. A diagram is oriented if every vertex has a linear ordering on its targets.

A node in the diagram is represented as a tuple: the indices of the level 0 nodes in the diagram that are reachable from this node, the indices of targets in the next lowest level, and the indices of the sources in the next highest level. The entire diagram is simply an array of arrays representing each particular level; index 0 represents level 0, etc.

Any directed graph can be encoded as an admissible Hasse diagram with 2 levels. The edges are level 1 and the vertices are level 0. The ordering on the targets of a node representing an edge is simply the terminal vertex first and the initial vertex second. This may be counterintuitive, but its helpful to interpret an arrow between two vertices as the "<" operator. This induces a linear ordering on the vertices of any acyclic complete subgraph - which is what the nodes in the Hasse diagram of the directed clique complex represent.

Any oriented simplicial complex can also be encoded as an admissible Hasse diagram. A node is a simplex, the targets are the faces of the simplex, and the sources are simplices of which the given simplex is a face.

The main feature of this module is an algorithm which takes the Hasse diagram of a directed graph and generates the Hasse diagram of the directed flag complex - the simplicial complex whose simplices are acyclic complete subgraphs of the given graph. Here acyclic refers to a directed graph without any sequence of arrows whose heads and tails match up an which has the same start and end vertex.

The idea is that, if your directed graph represents any kind of information flow, "sub-modules" in the network are groups of nodes that simply take input, process it, and then output it without spinning the information around at all. These "sub-modules" are the directed cliques/flags which I've been referring to as acyclic complete subgraphs up to this point. Constructing a simplicial complex out of them will allow you to both simplify the 1-dimensional topology of the network and possibly detect higher-dimensional topological features.

The algorithm for constructing the directed clique complex comes from this paper by Markram et al: https://www.frontiersin.org/articles/10.3389/fncom.2017.00048/full.

-}

module HasseDiagram
  ( Node
  , HasseDiagram
  , hsd2String
  , dGraph2sc
  , encodeDirectedGraph
  , directedFlagComplex
  , hDiagram2sc
  ) where

import Util
import SimplicialComplex

import Data.List as L
import Data.Vector as V

{- |
  Type representing a node in a Hasse diagram.
  Hasse diagrams are being used to represent simplicial complexes so each node represents a simplex.
  Contents of the tuple in order: Vector of references to vertices of the underlying directed graph,
  vector of references to the simplices faes in the next lowest level of the Hasse diagram,
  vector of references to "parent" simplices (simplices who have this simplex as a face) in the next highest level of the Hasse diagram.
-}
type Node = (Vector Int, Vector Int, Vector Int)

-- | Type representing an admissible Hasse diagram. Each entry in the vector represents a level in the Hasse diagram.
type HasseDiagram = Vector (Vector Node)

-- | Simple printing function for Hasse diagrams.
hsd2String :: HasseDiagram -> String
hsd2String =
  (L.intercalate "\n\n") . V.toList . (V.map (L.intercalate "\n" . V.toList . V.map show))

{- |
  Given the number of vertices in a directed graph,
  and pairs representing the direction of each edge,
  construct a 1-dimensional simplicial complex in the canonical way.
  Betti numbers of this simplicial complex can be used to count cycles and connected components.
-}
dGraph2sc :: Int -> [(Int, Int)] -> SimplicialComplex
dGraph2sc v edges =
  (v, V.fromList [V.fromList $ L.map (\(i, j) -> (i `cons` (j `cons` V.empty), V.empty)) edges])

{- |
  Given the number of vertices in a directed graph,
  and pairs representing the direction of each edge (initial, terminal),
  construct a Hasse diagram representing the graph.
-}
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

{- |
Given a Hasse diagram representing a directed graph, construct the diagram representing the directed clique/flag complex of the graph.
Algorithm adapted from the one shown in the supplementary materials of this paper: https://www.frontiersin.org/articles/10.3389/fncom.2017.00048/full
-}
directedFlagComplex :: HasseDiagram -> HasseDiagram
directedFlagComplex directedGraph =
  let edges    = V.last directedGraph
      fstSinks =
        V.map (\e ->
          V.map (\(e0, _) -> (two e0) ! 0) $
            findBothElems (\e1 e2 -> (two e1) ! 0 == (two e2) ! 0)
              (V.filter (\e0 -> (two e0) ! 1 == (two e) ! 1) edges)
                (V.filter (\e0 -> (two e0) ! 1 == (two e) ! 0) edges)) edges

      --take last level of nodes and their sinks
      --return modified last level, new level, and new sinks
      makeLevel :: Bool
                -> HasseDiagram
                -> Vector Node
                -> Vector (Vector Int)
                -> (Vector Node, Vector Node, Vector (Vector Int))
      makeLevel fstIter result oldNodes oldSinks =
        let maxindex = V.length oldNodes

            --given a node and a specific sink
            --construct a new node with new sinks that has the given index
            --Fst output is the modified input nodes
            --snd output is the new node, thrd output is the sinks of the new node
            makeNode :: Int
                     -> Int
                     -> Int
                     -> Vector Node
                     -> Vector Int
                     -> (Vector Node, Node, Vector Int)
            makeNode newIndex oldIndex sinkIndex nodes sinks =
              let sink     = sinks ! sinkIndex
                  oldNode  = nodes ! oldIndex
                  --the vertices of the new simplex are
                  --the vertices of the old simplex plus the sink
                  verts    = sink `cons` (one oldNode)
                  numFaces = V.length $ two oldNode

                  --find all the faces of the new node by looking at the faces of the old node
                  testTargets :: Int
                              -> Node
                              -> Vector Node
                              -> Node
                              -> Vector Int
                              -> (Vector Node, Node, Vector Int)
                  testTargets i onode onodes newNode newSinks =
                    let faceVerts =
                          if fstIter then one $ (V.last $ V.init $ result) ! ((two onode) ! i)
                          else one $ (V.last $ result) ! ((two onode) ! i)
                    in
                      if i == numFaces then (onodes, newNode, newSinks)
                      else
                        case V.find (\(_, (v, _, _)) ->
                               V.head v == sink && V.tail v == faceVerts)
                                 $ mapWithIndex (\j n -> (j, n)) onodes of
                          Just (j, n) ->
                            testTargets (i + 1) onode
                              (replaceElem j (one n, two n, (thr n) `smartSnoc` newIndex) onodes)
                                (one newNode, (two newNode) `snoc` j, thr newNode)
                                  (newSinks |^| (oldSinks ! j))
                          Nothing     -> error "HasseDiagram.directedFlagComplex.makeDiagram.makeNode.testTargets. This is a bug. Please email the Persistence maintainers."

              in testTargets 0 oldNode nodes (verts, oldIndex `cons` V.empty, V.empty) sinks

            loopSinks :: Int
                      -> Int
                      -> Vector Node
                      -> (Vector Node, Vector Node, Vector (Vector Int), Int)
            loopSinks nodeIndex lastIndex nodes =
              let node     = oldNodes ! nodeIndex
                  sinks    = oldSinks ! nodeIndex
                  numSinks = V.length sinks

                  loop i (modifiedNodes, newNodes, newSinks) =
                    if i == numSinks then (modifiedNodes, newNodes, newSinks, i + lastIndex)
                    else
                      let (modNodes, newNode, ns) =
                            makeNode (i + lastIndex) nodeIndex i modifiedNodes sinks
                      in loop (i + 1) (modNodes, newNodes `snoc` newNode, newSinks `snoc` ns)

              in loop 0 (nodes, V.empty, V.empty)

            loopNodes :: Int
                      -> Int
                      -> Vector Node
                      -> Vector Node
                      -> Vector (Vector Int)
                      -> (Vector Node, Vector Node, Vector (Vector Int))
            loopNodes i lastIndex nodes newNodes newSinks =
              if i == maxindex then (nodes, newNodes, newSinks)
              else
                let (modifiedNodes, nnodes, nsinks, index) = loopSinks i lastIndex nodes
                in loopNodes (i + 1) lastIndex modifiedNodes
                     (newNodes V.++ nnodes) (newSinks V.++ nsinks)

        in loopNodes 0 0 oldNodes V.empty V.empty

      loopLevels :: Int -> HasseDiagram -> Vector Node -> Vector (Vector Int) -> HasseDiagram
      loopLevels iter diagram nextNodes sinks =
        let (modifiedNodes, newNodes, newSinks) = makeLevel (iter < 2) diagram nextNodes sinks
            newDiagram                          = diagram `snoc` modifiedNodes
        in
          if V.null newNodes then newDiagram
          else loopLevels (iter + 1) newDiagram newNodes newSinks

  in loopLevels 0 directedGraph edges fstSinks

-- | Convert a Hasse diagram to a simplicial complex.
hDiagram2sc :: HasseDiagram -> SimplicialComplex
hDiagram2sc diagram =
  let sc = V.map (V.map not3) $ V.tail diagram
  in (V.length $ V.head diagram, (V.map (\(v, _) -> (v, V.empty)) $ sc ! 0) `cons` V.tail sc)
