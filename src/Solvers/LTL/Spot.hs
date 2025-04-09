module Solvers.LTL.Spot
  ( solve
  ) where

import           Control.Exception (bracket)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Process

-- | solve a LTL formula with spot (my script that does both the translation
-- to automata and then checks emptiness)
solve :: Formula -> IO SolverResult
solve f = callSpot (ltl2spot f)

-- convert an LTL formula to blacks format
ltl2spot :: Formula -> String
ltl2spot f = case f of
    Atom p        -> p
    Not f         -> "!(" <> ltl2spot f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> ltl2spot f <> ")"
    Future f      -> "F(" <> ltl2spot f <> ")"
    Globally f    -> "G(" <> ltl2spot f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> ltl2spot f1 <> ") " <> op <> " (" <> ltl2spot f2 <> ")"

-- make a call to spot_ltl_sat.py and parse the result
--
-- this is implemented using the "bracket pattern" which automatically manages
-- external resources (in this case creation and termination of the external solver)
callSpot :: String -> IO SolverResult
callSpot str = bracket
  (do
    -- start spot_ltl_sat.py
      (Just hin, Just hout, Just herr, handle) <- createProcess (proc "spot_ltl_sat.py" [str])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      pure (hin, hout, herr, handle)) -- startup
  (\(_, _, _, handle) -> do
    -- terminate the child process
    terminateProcess handle
    _ <- waitForProcess handle
    --putStrLn "terminating spot"
    pure ()
    ) -- cleanup
  (\(hin, hout, herr, handle) -> do
    -- read the results
    out <- hGetContents hout
    err <- hGetContents herr
    -- parse it
    case parseOut "sat" "unsat" out of
      Right b -> do
        putStrLn "formula solved by spot"
        pure $ Completed b
      Left _-> do
        err <- hGetContents herr
        let msg = "stdout: " <> out <> "\nstderr: " <> err
        pure $ Failed msg
    ) -- work

parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out    --
