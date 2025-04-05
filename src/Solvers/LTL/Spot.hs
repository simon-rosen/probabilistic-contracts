module Solvers.LTL.Spot
  ( solve
  ) where

import           Solvers.Solver
import           Specs.LTL
import           System.Process (readProcessWithExitCode)

-- | solve a LTL formula with spot (my script that does both the translation
-- to automata and then checks emptiness)
solve :: Formula -> IO SolverResult
solve f = callSpot (ltl2spot f)

-- convert an LTL formula to blacks format
ltl2spot :: Formula -> String
ltl2spot f = case f of
    Atom p        -> p
    Not f         -> "!(" <> ltl2spot f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> ltl2spot f <> ")"
    Future f      -> "F(" <> ltl2spot f <> ")"
    Globally f    -> "G(" <> ltl2spot f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> ltl2spot f1 <> ") " <> op <> " (" <> ltl2spot f2 <> ")"

-- call spot_ltl_sat.py and get a result
callSpot :: String -> IO SolverResult
callSpot str = do
  res <- readProcessWithExitCode "spot_ltl_sat.py" [str] ""
  pure $ parseRes "Spot" "sat" "unsat" res

