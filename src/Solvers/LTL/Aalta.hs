module Solvers.LTL.Aalta
  ( solve
  ) where
import           Control.Exception (bracket, evaluate, finally)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Process


-- | solve an LTL formula with aalta
solve :: Formula -> IO SolverResult
solve f = callAalta (show f)

-- make a call to aalta and parse the result
--
-- this is implemented using the "bracket pattern" which automatically manages
-- external resources (in this case creation and termination of the external solver)

callAalta :: String -> IO SolverResult
callAalta str = bracket
  -- startup code: start aalta
  ( createProcess (proc "aalta" [])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  )
  -- cleanup code: terminate process and close all handles
  (\(mhin, mhout, mherr, ph) -> do
    -- close all handles (safely)
    mapM_ safeClose [mhin, mhout, mherr]
    -- terminate the child process
    terminateProcess ph
    _ <- waitForProcess ph
    pure ()
  )
  -- worker code: send the input via stdin and parse the result
  (\(mhin, mhout, mherr, ph) -> do
    -- Write the formula string to Aalta's stdin, flush, and close to signal EOF.
    safeWriteAndClose mhin str
    -- read the results
    out <- safeReadAndClose mhout
    -- and any errors
    err <- safeReadAndClose mherr
    -- parse it
    case parseOut "sat" "unsat" out of
      Right b -> pure $ Completed b
      Left _-> do
        let msg = "aalta failed!\nstdout: " <> out <> "\nstderr: " <> err
        pure $ Failed msg
    ) -- work
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
    safeWriteAndClose h s = case h of
      Nothing -> pure ()
      Just h' -> do
        hPutStrLn h' s
        hFlush h'
        hClose h' `finally` pure ()



parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out

