import qualified Generate.LTL      as GenLTL
import qualified Generate.MLTL     as GenMLTL
import           Math
import           ProbContract
import qualified Solvers.LTL.Aalta as Aalta
import qualified Specs.LTL         as LTL
import qualified Specs.MLTL        as MLTL
import qualified Specs.Solvable    as Solvable


main :: IO ()
main = putStrLn "Test suite not yet implemented"

----------------- ltl ----------------------------
testRefinement :: IO ()
testRefinement = do
  -- system contract
  a0 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  g0 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  let c0 = mkProbContract (a0,g0) "<" 0.5
  -- component contract 1
  a1 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  g1 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  let c1 = mkProbContract (a1,g1) ">=" 0.6
  -- component contract 2
  a2 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  g2 <- GenLTL.generateFormula 100 ["a", "b", "c"]
  let c2 = mkProbContract (a2,g2) ">=" 0.6

  print [complement c0, c1, c2]

  --vars <- nonEmptyVars [c0, c1]
  --print vars
  --let ineqs = createIneqs [c0, c1] vars
  --let smt = toSMT vars ineqs
  --refines <- checkLineqs smt
  res <- refines 10000000 "portfolio" "z3" c0 [c1, c2]
  --putStrLn $ smt <> "\n\n"
  putStrLn $ case res of
                Right False -> "refinement result: unknown"
                Right True  -> "refinement result: refines!"
                Left err    -> "error in refinement process"

  --let psat = refinementReduction [c1] c2
  --print psat

testLTLsat = do
  f <- GenLTL.generateFormula 1000 ["a", "b", "c", "d", "e"]
  --putStrLn $ LTL.pretty f
  print $ Aalta.ltl2aalta f
  res <- Solvable.solveWithTimeout 10000000 "portfolio" f
  pure res

------------------------ mltl --------------------------------
testMLTLsat = do
  f <- GenMLTL.generateFormula 10 ["a", "b", "c", "d", "e"]
  putStrLn $ show f
  --print $ Aalta.ltl2aalta f
  res <- Solvable.solveWithTimeout 10000000 "portfolio" f
  pure res


