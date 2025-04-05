-- | Black is a symbolic LTL solver
module Solvers.LTL.Black
  ( solve
  ) where
import           Solvers.Solver
import           Specs.LTL
import           System.Process (readProcessWithExitCode)

-- | solve a LTL formula with black
solve :: Formula -> IO SolverResult
solve f = callBlack (ltl2black f)

-- convert an LTL formula to blacks format
ltl2black :: Formula -> String
ltl2black f = case f of
    Atom p        -> p
    Not f         -> "!(" <> ltl2black f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> ltl2black f <> ")"
    Future f      -> "F(" <> ltl2black f <> ")"
    Globally f    -> "G(" <> ltl2black f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> ltl2black f1 <> ") " <> op <> " (" <> ltl2black f2 <> ")"

-- call black and get a result
callBlack :: String -> IO SolverResult
callBlack str = do
  res <- readProcessWithExitCode "black" ["solve", "-f", str] ""
  pure $ parseRes "black" "SAT" "UNSAT" res

