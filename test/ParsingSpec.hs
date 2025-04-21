module ParsingSpec (spec) where

import qualified Contracts.Probabilistic as PC
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import qualified Generate.ProbContract   as GenProbContract
import           Parse.LTLParser         (parseLTL, parseLTLProbContract,
                                          parseLTLRefinementProblem)
import           Parse.MLTLParser        (parseMLTL, parseMLTLProbContract,
                                          parseMLTLRefinementProblem)
import qualified Specs.LTL               as LTL
import qualified Specs.MLTL              as MLTL

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | property based testing of parsing
-- using quickchecks monadic support: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html
spec :: Spec
spec = describe "Parsing of contracts, LTL formulas, etc" $ do
  it "randomly generated LTL formulas parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random formula
      f <- run (GenLTL.random 100 5)
      -- parse the pretty printed formula
      let p = parseLTL $ show f
      -- they should now be equal
      assert (f == p)

  it "randomly generated MLTL formulas parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random formula
      f <- run (GenMLTL.random 100 5 10)
      -- parse the pretty printed formula
      let p = parseMLTL $ show f
      -- they should now be equal
      assert (f == p)

  it "randomly generated (ProbContract LTL.Formula) parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random contract
      pc <- run (GenProbContract.randomWithLTL True 100 10)
      -- parse the pretty printed formula
      let pc' = parseLTLProbContract $ show pc
      -- they should now be equal
      assert (pc == pc')

  it "randomly generated (ProbContract MLTL.Formula) parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random contract
      pc <- run (GenProbContract.randomWithMLTL True 100 10 10)
      -- parse the pretty printed formula
      let pc' = parseMLTLProbContract $ show pc
      -- they should now be equal
      assert (pc == pc')

  it "randomly generated (RefinementProblem LTL.Formula) parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random contract
      rp <- run (GenProbContract.randomRefinementProblemWithLTL 5 50 10)
      -- parse the pretty printed formula
      let rp' = parseLTLRefinementProblem $ show rp
      -- they should now be equal
      assert (rp == rp')

  it "randomly generated (RefinementProblem MLTL.Formula) parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random contract
      rp <- run (GenProbContract.randomRefinementProblemWithMLTL 5 50 10 10)
      -- parse the pretty printed formula
      let rp' = parseMLTLRefinementProblem $ show rp
      -- they should now be equal
      assert (rp == rp')




