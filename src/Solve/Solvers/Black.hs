-- | Black is a symbolic LTL solver
module Solve.Solvers.Black
  ( solve
  ) where
import           Logics.LTL
import           System.Process (readProcess)

-- | solve a LTL formula with black
solve :: Formula -> IO (Either String Bool)
solve f = callBlack (ltl2black f)

-- convert an LTL formula to blacks format
ltl2black :: Formula -> String
ltl2black f = case f of
    Atom p        -> p
    Not f         -> "!(" <> pretty f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> pretty f <> ")"
    Future f      -> "F(" <> pretty f <> ")"
    Globally f    -> "G(" <> pretty f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> pretty f1 <> ") " <> op <> " (" <> pretty f2 <> ")"

-- call black and get a result
callBlack :: String -> IO (Either String Bool)
callBlack str = do
  res <- readProcess "black" ["solve", "-f", str] ""
  pure $ case res of
    "SAT\n"   -> Right True
    "UNSAT\n" -> Right False
    _         -> Left res

