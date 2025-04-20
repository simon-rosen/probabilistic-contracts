module ParsingSpec (spec) where

import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import           Parse.LTLParser         (parseLTL)
import           Parse.MLTLParser        (parseMLTL)
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
      let p = parseLTL $ LTL.pretty f
      -- they should now be equal
      assert (f == p)

  it "randomly generated MLTL formulas parses to themself when pretty printed" $ do
    monadicIO $ do
      -- generate a random formula
      f <- run (GenMLTL.random 100 5 10)
      -- parse the pretty printed formula
      let p = parseMLTL $ MLTL.pretty f
      -- they should now be equal
      assert (f == p)
