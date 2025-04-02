module Generate.LTL where
import           Specs.LTL
import           Test.QuickCheck

instance Arbitrary Formula where
  arbitrary = sized (\n -> genFormula n ["a", "b", "c", "d", "e"])

-- Modify genFormula to take depth and a list of atoms
genFormula :: Int -> [String] -> Gen Formula
genFormula 0 atoms = Atom <$> elements atoms
genFormula n atoms = oneof $
  [ --Atom <$> elements atoms
    Not <$> genFormula (n - 1) atoms
  , Or <$> genFormula (n `div` 2) atoms <*> genFormula (n `div` 2) atoms
  , And <$> genFormula (n `div` 2) atoms <*> genFormula (n `div` 2) atoms
  , Implies <$> genFormula (n `div` 2) atoms <*> genFormula (n `div` 2) atoms
  , Next <$> genFormula (n - 1) atoms
  , Future <$> genFormula (n - 1) atoms
  , Globally <$> genFormula (n - 1) atoms
  , Until <$> genFormula (n `div` 2) atoms <*> genFormula (n `div` 2) atoms
  ]

-- Helper function to generate formula with custom depth and atom list
generateFormula :: Int -> [String] -> IO Formula
generateFormula depth atoms = generate (genFormula depth atoms)


