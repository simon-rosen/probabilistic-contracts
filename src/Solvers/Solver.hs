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

-- | create functions to parse result from
-- calling a program with readProcessWithExitCode
parseRes :: String -> String -> String -> (ExitCode, String, String) -> SolverResult
parseRes solverName sat unsat (code, out, err)
  | unsat `isInfixOf` out = Completed False
  | sat `isInfixOf` out = Completed True
  | otherwise = Failed $ unlines
        [ solverName <> " failed!"
        , "exitcode: " <> show code
        , "stdout: " <> out
        , "stderr: " <> err
        ]

type Solver = Formula -> IO SolverResult

type AsyncSolver = Formula -> IO (IO SolverResult, IO ())
