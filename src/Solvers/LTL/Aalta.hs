module Solvers.LTL.Aalta
  ( solve
  ) where
import           Solvers.Solver
import           Specs.LTL
import           System.Process (readProcessWithExitCode)

-- | solve an LTL formula with aalta
solve :: Formula -> IO SolverResult
solve f = callAalta (ltl2aalta f)

-- convert an LTL formula to aalta syntax
ltl2aalta :: Formula -> String
ltl2aalta f = case f of
    Atom p        -> p
    Not f         -> "!(" <> ltl2aalta f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> ltl2aalta f <> ")"
    Future f      -> "F(" <> ltl2aalta f <> ")"
    Globally f    -> "G(" <> ltl2aalta f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> ltl2aalta f1 <> ") " <> op <> " (" <> ltl2aalta f2 <> ")"


-- make a call to aalta and parse the result
callAalta :: String -> IO SolverResult
callAalta str = do
  res <- readProcessWithExitCode "aalta" [] str
  pure $ parseRes "aalta" "sat" "unsat" res
