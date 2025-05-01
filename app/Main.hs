module Main (main) where

import           ArgParser
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import           Control.Concurrent.Timeout   (timeout)
import qualified Generate.ProbContract        as GenProb
import           Parse.LTLParser              (parseLTLRefinementProblem)
import           Parse.MLTLParser             (parseMLTLRefinementProblem)
import           Specs.Solvable

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
          verify p timeout

        FileInput path -> do
          str <- readFile path
          let p = parseLTLRefinementProblem str
          verify p timeout

      MLTL -> case inp of
        ArgInput str -> do
          let p = parseMLTLRefinementProblem str
          verify p timeout

        FileInput path -> do
          str <- readFile path
          let p = parseMLTLRefinementProblem str
          verify p timeout

    -- generating problems
    Generate generator -> case generator of
      GenerateLTL comps size atoms    -> do
        prob <- GenProb.randomRefinementProblemWithLTL comps size atoms
        putStrLn $ show prob

      GenerateMLTL comps size atoms time -> do
        prob <- GenProb.randomRefinementProblemWithMLTL comps size atoms time
        putStrLn $ show prob

    -- benchmarking the algorithm for problems
    Benchmark l inp timeout db -> case l of
      LTL -> case inp of
        ArgInput str -> do
          putStrLn "hi"
        FileInput path -> do
          putStrLn "hi"

      MLTL -> case inp of
        ArgInput str -> do
          putStrLn "hi"
        FileInput path -> do
          putStrLn "hi"



-- handler for the verify command
verify :: (Eq a, Solvable a) => RefinementProblem a -> Maybe Integer -> IO ()
verify p t =  case t of
  Nothing -> do
    res <- refines "portfolio" "z3" (systemContract p) (componentContracts p)
    handleRes res
  Just secs -> do
    res <- timeout (1000000*secs) $ refines "portfolio" "z3" (systemContract p) (componentContracts p)
    case res of
      Nothing  -> putStrLn "timeout"
      Just res -> handleRes res

  where handleRes res = case res of
          Left err    -> putStrLn $ "error: " <> show err
          Right False -> putStrLn "unknown"
          Right True  -> putStrLn "true"


-- handler for the benchmark command
benchmark :: 










