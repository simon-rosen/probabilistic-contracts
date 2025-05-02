module Solvers.LTL.Aalta
  ( solve
  ) where
import           Control.Exception (bracket, evaluate, finally)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Process

import           Control.Exception (SomeException, evaluate, try)
import           Data.List         (isInfixOf)
import           System.Exit       (ExitCode (..))
import           System.Process    (readProcessWithExitCode)


-- | solve an LTL formula with aalta
solve :: Formula -> IO SolverResult
solve f = callAalta (show f)


callAalta :: String -> IO SolverResult
callAalta input = do
  -- send the formula to stdin of Aalta, read stdout/stderr
  (exitCode, out, err) <- readProcessWithExitCode "aalta" [] input

  case exitCode of
    ExitFailure code ->
      pure $ Failed $ "aalta exited with code " <> show code <> "\nstderr: " <> err

    ExitSuccess -> do
      parsed <- try (evaluate (parseOut "sat" "unsat" out)) :: IO (Either SomeException (Either String Bool))
      case parsed of
        Right (Right b) -> pure $ Completed b
        Right (Left msg) -> pure $ Failed $ "Unrecognized output from aalta: " <> msg
        Left ex -> pure $ Failed $ "Exception while parsing Aalta output: " <> show ex


parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out

