module Main (main) where

import           ArgParser
import           Benchmark.Benchmark          (runLTLBenchmark,
                                               runMLTLBenchmark)
import           Benchmark.Database           (createLTLProblemsTable,
                                               createMLTLProblemsTable)
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import           Control.Concurrent.Timeout   (timeout)
import           Control.Monad                (forM_)
import           Database.SQLite.Simple       (Connection, close, open)
import qualified Generate.ProbContract        as GenProb
import           Parse.LTLParser              (parseLTLRefinementProblem)
import           Parse.MLTLParser             (parseMLTLRefinementProblem)
import           Specs.Solvable
import           System.Process               (readProcessWithExitCode)

main :: IO ()
main = do
  args <- parseArgs
  -- matching on args
  case args of
    -- verifying refinement
    Verify l inp timeout -> case l of
      LTL -> case inp of
        ArgInput str -> do
          let p = parseLTLRefinementProblem str
          verify "portfolio" p timeout

        FileInput path -> do
          str <- readFile path
          let p = parseLTLRefinementProblem str
          verify "portfolio" p timeout

      MLTL -> case inp of
        ArgInput str -> do
          let p = parseMLTLRefinementProblem str
          verify "smt" p timeout

        FileInput path -> do
          str <- readFile path
          let p = parseMLTLRefinementProblem str
          verify "smt" p timeout

    -- generating problems
    Generate generator -> case generator of
      GenerateLTL comps size atoms    -> do
        prob <- GenProb.randomRefinementProblemWithLTL comps size atoms
        putStrLn $ show prob

      GenerateMLTL comps size atoms time -> do
        prob <- GenProb.randomRefinementProblemWithMLTL comps size atoms time
        putStrLn $ show prob

    -- benchmarking the algorithm for problems
    Benchmark benchmarker -> case benchmarker of
      BenchmarkerLTL comps size atoms db t -> do
        conn <- open db
        createLTLProblemsTable conn
        runLTLBenchmark conn comps size atoms t t
        killProcesses
        close conn

      BenchmarkerMLTL comps size atoms maxt db t -> do
        conn <- open db
        createMLTLProblemsTable conn
        runMLTLBenchmark conn comps size atoms maxt t t
        killProcesses
        close conn



-- handler for the verify command
verify :: (Eq a, Solvable a) => String -> RefinementProblem a -> Maybe Integer -> IO ()
verify solver p t =  case t of
  Nothing -> do
    res <- refines solver "z3" (systemContract p) (componentContracts p)
    handleRes res
  Just secs -> do
    res <- timeout (1000000*secs) $ refines solver "z3" (systemContract p) (componentContracts p)
    case res of
      Nothing  -> do
        killProcesses
        putStrLn "timeout"
      Just res -> handleRes res

  where handleRes res = case res of
          Left err    -> putStrLn $ "error: " <> show err
          Right False -> putStrLn "unknown"
          Right True  -> putStrLn "true"



-- used to kill all external processes started if a timeout occurs
-- because then this code might not get called in their handlers.
killProcesses :: IO ()
killProcesses = do
  forM_ ["aalta", "black", "spot_ltl_sat.py", "z3", "MLTLConvertor"] $ \name -> do
    _ <- readProcessWithExitCode "pkill" ["-9", "-f", name] ""
    pure ()








