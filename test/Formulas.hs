module Formulas where
import           Contracts.Probabilistic
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import qualified Generate.ProbContract   as GenProbContract
import           Math
import           Parse.LTLParser         (parseLTLProbContract,
                                          parseLTLRefinementProblem)
import           Parse.MLTLParser        (parseMLTLProbContract,
                                          parseMLTLRefinementProblem)
import qualified Specs.LTL               as LTL
import qualified Specs.MLTL              as MLTL
import qualified Specs.Solvable          as Solvable


testLTLsat = do
  f <- GenLTL.random 1000 10
  print f
  res <- Solvable.solveWithTimeout 10000000 "portfolio" f
  pure res

testMLTLsat = do
  f <- GenMLTL.random 20 5 10
  --putStrLn $ LTL.pretty f
  print f
  ltlF <- MLTL.toLTL f
  putStrLn "converted to LTL:"
  print ltlF
  res <- Solvable.solveWithTimeout 10000000 "portfolio" f
  pure res

testProbContract = do
  pc <- GenProbContract.randomWithLTL True 100 5
  print pc
  let pc' = parseLTLProbContract $ show pc
  print pc'
  print $ pc == pc'

testMLTLProbContract = do
  pc <- GenProbContract.randomWithMLTL True 100 5 20
  print pc
  let pc' = parseMLTLProbContract $ show pc
  print pc'
  print $ pc == pc'

testLTLProblem = do
  rp <- GenProbContract.randomRefinementProblemWithLTL 3 5 5
  print rp
  let rp' = parseLTLRefinementProblem $ show rp
  print rp'
  print $ rp == rp'

testMLTLProblem = do
  rp <- GenProbContract.randomRefinementProblemWithMLTL 3 5 5 10
  print rp
  let rp' = parseMLTLRefinementProblem $ show rp
  print rp'
  print $ rp == rp'
