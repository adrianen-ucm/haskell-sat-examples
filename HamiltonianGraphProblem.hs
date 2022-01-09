-- |
-- Module      : HamiltonianGraphProblem
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
module HamiltonianGraphProblem where

import Graph
import PropProblemSmtLib

-- | Hamiltonian graph problem expressed as a propositional logic problem.
hamiltonianGraphProblem :: Graph -> PropProblem
hamiltonianGraphProblem graph =
  let n = numNodes graph
      nodeInPosition i j = Var $ "p" <> show i <> "_" <> show j
   in PropProblem
        { name = "Hamiltonian graph",
          description =
            unlines
              [ "This script checks if the undirected graph with edges",
                "\t" <> show graph,
                "is Hamiltonian (i.e. it has a Hamiltonian path).",
                "The propositional variable 'pi_j' means that the node i appears in the path position j,",
                "where nodes are labeled as natural numbers starting from 0."
              ],
          constraints =
            [ "Every node should appear in at least one position."
                =| [ Or . map (nodeInPosition i) $ [0 .. n - 1]
                     | i <- [0 .. n - 1]
                   ],
              "Two different nodes do not appear in the same path position."
                =| [ Not . And $ [nodeInPosition i k, nodeInPosition j k]
                     | i <- [0 .. n - 1],
                       j <- [0 .. n - 1],
                       i /= j,
                       k <- [0 .. n - 1]
                   ],
              "If two nodes are not adjacent, then they do not appear consecutively in the path."
                =| [ (nodeInPosition i k ~>) . Not . nodeInPosition j $ k + 1
                     | i <- [0 .. n - 1],
                       j <- [0 .. n - 1],
                       i /= j,
                       not . hasEdge (i, j) $ graph,
                       k <- [0 .. n - 2]
                   ]
            ]
        }
