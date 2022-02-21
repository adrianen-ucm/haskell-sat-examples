module Main where

import           SatExample.HamiltonianPathProblem
import           SatExample.PropProblemSmtLib

-- | Example usage, having z3 and ghc installed:
-- z3 <(runghc hamiltonian-path-sat.hs)
--
-- You can also save the generated SMT-LIB code as follows:
-- runghc hamiltonian-path-sat.hs > my-script.smt
--
-- and also change the input graph as needed.
main :: IO ()
main =
  putStrLn . checkSatScript $
    hamiltonianPathProblem
      [ (0, 1),
        (1, 2),
        (2, 3),
        (3, 4),
        (4, 5),
        (1, 3)
      ]
