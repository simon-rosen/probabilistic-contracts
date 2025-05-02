module Solvers.LTL.Spot
  ( solve
  , callSpot
  ) where

import           Control.Exception (bracket, evaluate, finally)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Process

import           Control.Exception (SomeException, evaluate, try)
import           System.Exit       (ExitCode (..))
import           System.Process    (readProcessWithExitCode)

-- | solve a LTL formula with spot (my script that does both the translation
-- to automata and then checks emptiness)
solve :: Formula -> IO SolverResult
solve f = callSpot (show f)

callSpot :: String -> IO SolverResult
callSpot formulaStr = do
  (exitCode, out, err) <- readProcessWithExitCode "spot_ltl_sat.py" [] (show formulaStr)

  case exitCode of
    ExitFailure code ->
      pure $ Failed $
        "spot_ltl_sat.py exited with code " <> show code <> "\nstderr: " <> err

    ExitSuccess -> do
      parsed <- try (evaluate (parseOut "sat" "unsat" out)) :: IO (Either SomeException (Either String Bool))
      case parsed of
        Right (Right b) -> pure $ Completed b
        Right (Left msg) -> pure $ Failed $ "Unrecognized output from spot_ltl_sat.py: " <> msg
        Left ex -> pure $ Failed $ "Exception while parsing Spot output: " <> show ex


parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out

