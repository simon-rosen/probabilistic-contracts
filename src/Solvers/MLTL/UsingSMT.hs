-- | this solver uses a reduction to first order logic and solves this
-- problem using the MLTLConvertor tool with the "-smtlib" flag, which converts
-- the problem to smtlib format, then it calls z3 to solve it.
module Solvers.MLTL.UsingSMT where

import           Control.Exception (SomeException, bracket, evaluate, finally,
                                    try)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.MLTL
import           System.Exit       (ExitCode (..))
import           System.IO
import           System.Process
import           System.Process    (readProcessWithExitCode)

-- | Solve an MLTL formula by converting it to SMT-LIB and passing it to Z3
solve :: Formula -> IO SolverResult
solve f = do
  -- Step 1: Convert MLTL to SMT-LIB format using MLTLConvertor
  (exitConvert, smtlib, convertErr) <- readProcessWithExitCode "MLTLConvertor" ["-smtlib", (show f)] ""

  case exitConvert of
    ExitFailure code ->
      pure $ Failed $ "MLTLConvertor failed with code " ++ show code ++ "\n" ++ convertErr
    ExitSuccess -> do
      -- Step 2: Pass the SMT-LIB to Z3
      (exitZ3, out, err) <- readProcessWithExitCode "z3" ["-in"] smtlib

      case exitZ3 of
        ExitFailure code ->
          pure $ Failed $ "Z3 exited with code " ++ show code ++ "\nstderr: " ++ err
        ExitSuccess -> do
          parsed <- try (evaluate (parseOut "sat" "unsat" out)) :: IO (Either SomeException (Either String Bool))
          case parsed of
            Right (Right b) -> pure $ Completed b
            Right (Left msg) -> pure $ Failed $ "Unrecognized output from Z3:\n" ++ msg
            Left ex -> pure $ Failed $ "Exception while parsing Z3 output: " ++ show ex


