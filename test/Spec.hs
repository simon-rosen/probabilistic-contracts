import qualified Generate.LTL as GenLTL
import           Math
import           ProbContract
import qualified Specs.LTL    as LTL
import qualified Specs.Spec   as Spec


main :: IO ()
main = putStrLn "Test suite not yet implemented"

testRefinement :: IO ()
testRefinement = do
  -- contract 1
  a0 <- GenLTL.generateFormula 5 ["a", "b"]
  g0 <- GenLTL.generateFormula 5 ["a", "b"]
  let c0 = mkProbContract (a0,g0) "<" 0.5
  -- contract 2
  a1 <- GenLTL.generateFormula 5 ["a", "b"]
  g1 <- GenLTL.generateFormula 5 ["a", "b"]
  let c1 = mkProbContract (a1,g1) ">=" 0.6
  print [complement c0, c1]

  --vars <- nonEmptyVars [c0, c1]
  --print vars
  --let ineqs = createIneqs [c0, c1] vars
  --let smt = toSMT vars ineqs
  --refines <- checkLineqs smt
  res <- refines "aalta" "z3" c0 [c1]
  --putStrLn $ smt <> "\n\n"
  putStrLn $ show res

  --let psat = refinementReduction [c1] c2
  --print psat

testLTLsat = do
  f <- GenLTL.generateFormula 20 ["a", "b", "c"]
  --putStrLn $ LTL.pretty f
  print f
  res <- Spec.solve "black" f
  pure res

