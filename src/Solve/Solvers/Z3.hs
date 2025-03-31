-- | Z3 is a SMT solver
module Solve.Solvers.Z3
  ( solve
  ) where

import           Math
import           System.IO      (hClose, hPutStr)
import           System.IO.Temp (withSystemTempFile)

solve :: [Var] -> [Var] -> [LinearIneq] -> IO (Either String Bool)
solve empty nonempty ineqs = checkLineqs (toSMT empty nonempty ineqs)

-- convert a set of linear inequalities to smt2 format
toSMT :: [Var] -> [Var] -> [LinearIneq] -> String
toSMT empty nonempty ineqs =
  -- specify that we use quantifier free LRA
  "(set-logic QF_LRA)\n" <>
  -- declare variables to be reals
  concatMap (\var -> "(declare-const " <> show var <> " Real)\n") nonempty <>
  -- show all linear inequalities
  concatMap (\ineq -> case ineq of
    LeftSum vs r p -> "(assert (" <> show r <> " (+" <> concatMap (\v -> " z" <> show v) vs <> ") " <> show p <> "))\n"
    Individual v r p -> "(assert (" <> show r <>" z" <> show v <> " " <> show p <> "))\n"
    LeftAndScaledRightSum vsl r p vsr ->
      -- left side, added a 0 so that the equation makes when no variables are present
      "(assert (" <> show r <> " (+ 0" <> concatMap (\v -> " z" <> show v) vsl <> ") " <>
      -- right side
        "(* " <> show p <> " (+ 0" <> concatMap (\v -> " z" <> show v) vsr <> "))))\n"
    ) ineqs <>
  -- instruct solver to check satisfiability
  "(check-sat)"

checkLineqs :: String -> IO (Either String Bool)
checkLineqs smt = do
  withSystemTempFile "lineqs.smt2" $ \tmpFile tmpHandle -> do
    hPutStr tmpHandle smt
    hClose tmpHandle
    res <- readProcess "z3" [tmpFile] ""
    -- print res
    pure $ case res of
      "sat\n"   -> Right True
      "unsat\n" -> Right False
      _         -> Left res

