module Main where

import HamiltonianGraphProblem
import PropProblemSmtLib

-- | Example usage, having z3 and ghc installed:
-- z3 <(runghc hamiltonian-graph-sat.hs)
--
-- You can also save the generated SMT-LIB code as follows:
-- runghc hamiltonian-graph-sat.hs > my-script.smt
--
-- and also change the input graph as needed.
main :: IO ()
main =
  putStrLn . checkSatScript $
    hamiltonianGraphProblem
      [ (0, 1),
        (1, 2),
        (2, 3),
        (3, 4),
        (4, 5),
        (1, 3)
      ]
