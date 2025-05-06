-- | All solvers will return similar results
module Solvers.Solver where
import           Data.List   (isInfixOf)
import           Specs.LTL
import           System.Exit (ExitCode)

-- | An external solver either completes and returns a bool,
-- fails and returns an error message, or is aborted.
data SolverResult = Completed Bool
                  | Failed String
                  | TimedOut
                  deriving (Eq, Show)

type Solver = Formula -> IO SolverResult


parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out
