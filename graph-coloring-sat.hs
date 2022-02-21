module Main where

import           SatExample.GraphColoringProblem
import           SatExample.PropProblemSmtLib

-- | Example usage, having z3 and ghc installed:
-- z3 <(runghc graph-coloring-sat.hs)
--
-- You can also save the generated SMT-LIb code as follows:
-- runghc graph-coloring-sat.hs > my-script.smt
--
-- and also change the input graph as needed.
main :: IO ()
main =
  putStrLn . checkSatScript $
    graphColoringProblem
      3
      [ (0, 1),
        (1, 2),
        (2, 3),
        (3, 4),
        (4, 5),
        (5, 1),
        (1, 4),
        (2, 4)
      ]
