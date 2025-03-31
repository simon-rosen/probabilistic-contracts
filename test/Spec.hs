import qualified Generate.LTL as GenLTL
import qualified Logics.LTL   as LTL
import           ProbContract


main :: IO ()
main = putStrLn "Test suite not yet implemented"

testRefinement :: IO ()
testRefinement = do
  -- contract 1
  a0 <- GenLTL.generateFormula 3 ["a", "b"]
  g0 <- GenLTL.generateFormula 3 ["a", "b"]
  let c0 = mkProbContract (a0,g0) "<" 0.5
  -- contract 2
  a1 <- GenLTL.generateFormula 3 ["a", "b"]
  g1 <- GenLTL.generateFormula 3 ["a", "b"]
  let c1 = mkProbContract (a1,g1) ">=" 0.6

  --vars <- nonEmptyVars [c0, c1]
  --print vars
  --let ineqs = createIneqs [c0, c1] vars
  --let smt = toSMT vars ineqs
  --refines <- checkLineqs smt
  res <- refines [c1] c0
  --putStrLn $ smt <> "\n\n"
  putStrLn $ show res

  --let psat = refinementReduction [c1] c2
  --print psat

testLTLsat = do
  f <- GenLTL.generateFormula 20 ["a", "b", "c"]
  putStrLn $ LTL.pretty f
  res <- LTL.sat f
  pure res

