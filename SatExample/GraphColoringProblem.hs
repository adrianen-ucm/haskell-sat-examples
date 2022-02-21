-- |
-- Module      : SatExample.GraphColoringProblem
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
module SatExample.GraphColoringProblem where

import           SatExample.Graph
import           SatExample.PropProblemSmtLib

-- | Graph coloring problem expressed as a propositional logic problem.
graphColoringProblem :: Int -> Graph -> PropProblem
graphColoringProblem numColors graph =
  let n = numNodes graph
      nodeHasColor i j = Var $ "p" <> show i <> "_" <> show j
   in PropProblem
        { name = "Graph coloring",
          description =
            unlines
              [ "This script checks if the graph with edges",
                "\t" <> show graph,
                "can be colored with " <> show numColors <> " colors with adjacent nodes not sharing color.",
                "The propositional variable 'pi_j' means that the node i is colored with j,",
                "where nodes and colors are labeled as natural numbers starting from 0."
              ],
          constraints =
            [ "Every node should have a color."
                =| [ Or . map (nodeHasColor i) $ [0 .. numColors - 1]
                     | i <- [0 .. n - 1]
                   ],
              "If some node has a color, then it has no other color."
                =| [ nodeHasColor i j
                       ~> And
                         [ Not $ nodeHasColor i k
                           | k <- [0 .. numColors - 1],
                             k /= j
                         ]
                     | i <- [0 .. n - 1],
                       j <- [0 .. numColors - 1]
                   ],
              "If some node has a color, then no one of its neighbours has the same color."
                =| [ (nodeHasColor i k ~>) . Not . nodeHasColor j $ k
                     | (i, j) <- graph,
                       i /= j,
                       k <- [0 .. numColors - 1]
                   ]
            ]
        }
