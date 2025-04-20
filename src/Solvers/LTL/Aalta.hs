module Solvers.LTL.Aalta
  ( solve
  ) where
import           Control.Exception        (bracket, evaluate)
import           Data.List                (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import           System.IO
import           System.Posix.Signals
import           System.Process
import           System.Process.Internals


-- | solve an LTL formula with aalta
solve :: Formula -> IO SolverResult
solve f = callAalta (show f)

-- make a call to aalta and parse the result
--
-- this is implemented using the "bracket pattern" which automatically manages
-- external resources (in this case creation and termination of the external solver)

callAalta :: String -> IO SolverResult
callAalta str = bracket
  (do
    -- start aalta
      (Just hin, Just hout, Just herr, handle) <- createProcess (proc "aalta" [])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      pure (hin, hout, herr, handle)) -- startup
  (\(hin, hout, herr, handle) -> do
    -- terminate the child process
    terminateProcess handle
    --_ <- waitForProcess handle
    --putStrLn "terminating aalta"
    pure ()
    ) -- cleanup
  (\(hin, hout, herr, handle) -> do
    -- Write the formula string to Aalta's stdin, flush, and close to signal EOF.
    hPutStrLn hin str
    hFlush hin
    hClose hin
    -- read the results
    out <- hGetContents hout
    _   <- evaluate (length out)  -- force complete evaluation of the output
    err <- hGetContents herr
    _   <- evaluate (length err)  -- force complete evaluation of the error stream
    -- parse it
    case parseOut "sat" "unsat" out of
      Right b -> pure $ Completed b
      Left _-> do
        err <- hGetContents herr
        let msg = "aalta failed!\nstdout: " <> out <> "\nstderr: " <> err
        pure $ Failed msg
    ) -- work


parseOut :: String -> String -> String -> Either String Bool
parseOut sat unsat out
  | unsat `isInfixOf` out = Right False
  | sat `isInfixOf` out = Right True
  | otherwise = Left out

{-
-- | Spawn the Aalta process, send the input formula via stdin,
-- read its stdout and stderr strictly, and parse the result.
solve :: Formula -> IO SolverResult
solve formula =
  withCreateProcess (proc "aalta" [])
    { std_in  = CreatePipe, std_out = CreatePipe, std_err = CreatePipe } $ \mbIn mbOut mbErr ph -> do
      hin  <- maybe (error "Aalta: missing stdin") return mbIn
      hout <- maybe (error "Aalta: missing stdout") return mbOut
      herr <- maybe (error "Aalta: missing stderr") return mbErr

      -- Write the formula string to Aalta's stdin, flush, and close to signal EOF.
      hPutStrLn hin (ltl2aalta formula)
      hFlush hin
      hClose hin

      -- Read the output strictly.
      out <- hGetContents hout
      _   <- evaluate (length out)  -- force complete evaluation of the output
      err <- hGetContents herr
      _   <- evaluate (length err)  -- force complete evaluation of the error stream

      _ <- waitForProcess ph

      case parseOut "sat" "unsat" out of
        Right b -> do
          putStrLn "formula solved by aalta"
          pure $ Completed b
        Left _ -> pure $ Failed ("aalta erros!\n out: " <> out <> "\nerr:" <> err <> "\n")

-}
