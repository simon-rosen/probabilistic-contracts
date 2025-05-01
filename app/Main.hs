module Main (main) where
import           ArgParser
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import           Parse.LTLParser              (parseLTLRefinementProblem)
import           Parse.MLTLParser             (parseMLTLRefinementProblem)

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Verify l inp -> case l of
      LTL -> case inp of
        ArgInput str -> do
          let p = parseLTLRefinementProblem str
          res <- refines 10000000 "portfolio" "z3" (systemContract p) (componentContracts p)
          case res of
            Left err    -> putStrLn $ "error: " <> show err
            Right False -> putStrLn "unknown"
            Right True  -> putStrLn "true"
        FileInput path -> do
          str <- readFile path
          let ltl = parseLTLRefinementProblem str
          print ltl
      MLTL -> case inp of
        ArgInput str -> do
          let p = parseMLTLRefinementProblem str
          res <- refines 10000000 "portfolio" "z3" (systemContract p) (componentContracts p)
          case res of
            Left err    -> putStrLn $ "error: " <> show err
            Right False -> putStrLn "unknown"
            Right True  -> putStrLn "true"
        FileInput path -> do
          str <- readFile path
          let p = parseMLTLRefinementProblem str
          print p
    Generate l -> case l of
      LTL  -> putStrLn "generate --lang LTL not implemented"
      MLTL -> putStrLn "generate --lang MLTL not implemented"


