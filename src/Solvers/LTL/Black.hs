-- | Black is a symbolic LTL solver
module Solvers.LTL.Black
  ( solve
  ) where
import           Control.Exception (bracket)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Process

-- | solve a LTL formula with black
solve :: Formula -> IO SolverResult
solve f = callBlack (ltl2black f)

-- convert an LTL formula to blacks format
ltl2black :: Formula -> String
ltl2black f = case f of
    Atom p        -> p
    Not f         -> "!(" <> ltl2black f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> ltl2black f <> ")"
    Future f      -> "F(" <> ltl2black f <> ")"
    Globally f    -> "G(" <> ltl2black f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> ltl2black f1 <> ") " <> op <> " (" <> ltl2black f2 <> ")"

-- make a call to black and parse the result
--
-- this is implemented using the "bracket pattern" which automatically manages
-- external resources (in this case creation and termination of the external solver)
callBlack :: String -> IO SolverResult
callBlack str = bracket
  (do
    -- start aalta
      (Just hin, Just hout, Just herr, handle) <- createProcess (proc "black" ["solve", "-f", str])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      pure (hin, hout, herr, handle)) -- startup
  (\(_, _, _, handle) -> do
    -- terminate the child process
    terminateProcess handle
    _ <- waitForProcess handle
    --putStrLn "terminating black"
    pure ()
    ) -- cleanup
  (\(hin, hout, herr, handle) -> do
    -- read the results
    out <- hGetContents hout
    err <- hGetContents herr
    -- parse it
    case parseOut "SAT" "UNSAT" out of
      Right b -> do
        putStrLn "formula solved by black"
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
  | otherwise = Left out
