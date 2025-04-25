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
          print res
        FileInput path -> do
          str <- readFile path
          let ltl = parseLTLRefinementProblem str
          print ltl
      MLTL -> case inp of
        ArgInput str -> do
          let ltl = parseMLTLRefinementProblem str
          print ltl
        FileInput path -> do
          str <- readFile path
          let p = parseMLTLRefinementProblem str
          print p
    Generate l -> case l of
      LTL  -> putStrLn "generate --lang LTL not implemented"
      MLTL -> putStrLn "generate --lang MLTL not implemented"


