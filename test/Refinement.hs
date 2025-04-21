module Refinement where
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import qualified Generate.LTL                 as GenLTL
import qualified Generate.MLTL                as GenMLTL
import           Math
import qualified Specs.LTL                    as LTL
import qualified Specs.MLTL                   as MLTL
import qualified Specs.Solvable               as Solvable



----------------- ltl ----------------------------
testRefinement :: IO ()
testRefinement = do
  -- system contract
  a0 <- GenLTL.random 100 5
  g0 <- GenLTL.random 100 5
  let c0 = mkProbContract (a0,g0) "<" 0.5
  -- component contract 1
  a1 <- GenLTL.random 100 5
  g1 <- GenLTL.random 100 5
  let c1 = mkProbContract (a1,g1) ">=" 0.6
  -- component contract 2
  a2 <- GenLTL.random 100 5
  g2 <- GenLTL.random 100 5
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
  f <- GenLTL.random 1000 10
  print $ f
  res <- Solvable.solveWithTimeout 10000000 "portfolio" f
  pure res

