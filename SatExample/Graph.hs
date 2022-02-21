-- |
-- Module      : SatExample.Graph
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module contains a very simple representation of a graph data structure.
module SatExample.Graph where

import           Numeric.Natural

-- | A graph represented as a list of edges (i.e. node pairs) where
-- nodes are natural numbers starting from 0.
type Graph = [(Natural, Natural)]

-- | Number of nodes in a graph.
numNodes :: Graph -> Natural
numNodes = fromIntegral . maximum . map ((+ 1) . uncurry max)

-- | Checks if an undirected graph contains an edge.
hasEdge :: (Natural, Natural) -> Graph -> Bool
hasEdge (i, j) graph = (i, j) `elem` graph || (j, i) `elem` graph
