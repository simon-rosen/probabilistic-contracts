module Formulas where
import           Contracts.Probabilistic
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import           Math
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

