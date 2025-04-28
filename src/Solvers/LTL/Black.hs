-- | Black is a symbolic LTL solver
module Solvers.LTL.Black
  ( solve
  , callBlack
  ) where
import           Control.Exception (bracket, evaluate, finally)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.Exit       (ExitCode (..))
import           System.IO
import           System.Process

-- | solve a LTL formula with black
solve :: Formula -> IO SolverResult
solve f = callBlack (show f)

-- make a call to black and parse the result
--
-- this is implemented using the "bracket pattern" which automatically manages
-- external resources (in this case creation and termination of the external solver)
callBlack :: String -> IO SolverResult
callBlack str = bracket
  -- startup code: start black
  (createProcess (proc "black" ["solve", "-f", str])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  -- cleanup code: terminate the process and close all handles
  (\(mhin, mhout, mherr, ph) -> do
    -- close all handles (safely)
    mapM_ safeClose [mhin, mhout, mherr]
    -- terminate the child process
    terminateProcess ph
    _ <- waitForProcess ph
    pure ()
  )
  -- worker code: run the solver and interpret the result
  (\(_mhin, mhout, mherr, ph) -> do
    -- read the results
    out <- safeReadAndClose mhout
    -- and any errors
    err <- safeReadAndClose mherr
    -- parse it
    case parseOut "SAT" "UNSAT" out of
      Right b -> pure $ Completed b
      Left _-> do
        let msg = "stdout: " <> out <> "\nstderr: " <> err
        pure $ Failed msg
  )
  where
    -- safely close a process handle
    safeClose h = case h of
      Nothing -> pure ()
      Just h' -> hClose h' `finally` pure ()
    -- safely read the contents from a file (strictly) handle and close it
    safeReadAndClose h = case h of
      Nothing -> pure ""
      Just h' -> do
        out <- hGetContents h'
        -- make this a strict IO operation
        _   <- evaluate (length out)
        hClose h'
        pure out



parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out
