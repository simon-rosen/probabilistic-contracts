-- | Z3 is a SMT solver
module Solvers.LinearEq.Z3
  ( solve
  ) where

import           Contracts.Refinement.Reductions.LinearEq
import           Math
import           System.Exit                              (ExitCode (..))
import           System.IO                                (hClose, hPutStr)
import           System.IO.Temp                           (withSystemTempFile)
import           System.Process                           (readProcessWithExitCode)
import           Text.Printf                              (printf)

solve :: [LinearEq] -> IO (Either String Bool)
solve ineqs = checkLineqs (toSMT ineqs)

-- convert a set of linear inequalities to smt2 format
toSMT :: [LinearEq] -> String
toSMT ineqs =
  -- specify that we use quantifier free LRA
  "(set-logic QF_LRA)\n" <>
  -- declare variables to be reals
  concatMap (\var -> "(declare-const " <> show var <> " Real)\n") (getVars ineqs) <>
  -- show all linear inequalities
  concatMap (\ineq -> case ineq of
    SumTo1 vars -> "(assert (= " <> " (+" <> concatMap (\v -> " " <> show v) vars <> ") 1.0))\n"
    NonNeg var -> "(assert (>= "  <> show var <> " 0.0))\n"
    ContractEq vsl c p vsr ->
      -- left side, added a 0 so that the equation makes when no variables are present
      "(assert (" <> show c <> " (+ 0" <> concatMap (\v -> " " <> show v) vsl <> ") " <>
      -- right side
        "(* " <> showDouble p <> " (+ 0" <> concatMap (\v -> " " <> show v) vsr <> "))))\n"
    ContractEqProd vsl c ps vsr ->
      -- left side, added a 0 so that the equation makes when no variables are present
      "(assert (" <> show c <> " (+ 0" <> concatMap (\v -> " " <> show v) vsl <> ") " <>
      -- right side (productlist is expanded to "(* p1 p2 ...)")
        "(* " <> unwords (map showDouble ps) <> " (+ 0" <> concatMap (\v -> " " <> show v) vsr <> "))))\n"
    ) ineqs <>
  -- instruct solver to check satisfiability
  "(check-sat)"
  where
    -- we dont want to print with scientific notation
    showDouble :: Double -> String
    showDouble d = printf "%.16f" d

checkLineqs :: String -> IO (Either String Bool)
checkLineqs smt = do
  withSystemTempFile "lineqs.smt2" $ \tmpFile tmpHandle -> do
    hPutStr tmpHandle smt
    hClose tmpHandle
    --putStrLn smt
    --res <- readProcess "z3" [tmpFile] ""
    -- print res
    --pure $ case res of
    --  "sat\n"   -> Right True
    --  "unsat\n" -> Right False
    --  _         -> Left res
    -- run Z3, capturing exit code, stdout, and stderr
    (ec, out, err) <- readProcessWithExitCode "z3" [tmpFile] ""

    case ec of
      ExitSuccess ->
        case out of
          "sat\n"   -> pure (Right True)
          "unsat\n" -> pure (Right False)
          other     -> pure (Left $ "Unexpected Z3 output:\n" ++ other)

      ExitFailure _ -> do
        putStrLn smt
        -- include both stdout and stderr in the error message
        pure (Left $ "Z3 failed with:\n" ++ "stdout:\n" ++ out ++ "stderr:\n" ++ err)

