-- | Black is a symbolic LTL solver
module Solvers.LTL.Black
  ( solve
  ) where
import           Control.Exception (SomeException, bracket, evaluate, finally,
                                    try)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.Exit       (ExitCode (..))
import           System.IO
import           System.Process
import           System.Process    (readProcessWithExitCode)

-- | solve a LTL formula with black
solve :: Formula -> IO SolverResult
solve f = do
  (exitCode, out, err) <- readProcessWithExitCode "black" ["solve", "-"] (show f)

  case exitCode of
    ExitFailure code ->
      pure $ Failed $ "black exited with code " <> show code <> "\n" <> err

    ExitSuccess -> do
      parsed <- try (evaluate (parseOut "SAT" "UNSAT" out)) :: IO (Either SomeException (Either String Bool))
      case parsed of
        Right (Right b) -> pure $ Completed b
        Right (Left msg) -> pure $ Failed $ "Unrecognized output from black: " <> msg
        Left ex -> pure $ Failed $ "Exception while parsing black output: " <> show ex


