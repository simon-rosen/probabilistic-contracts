module Logics.PropositionalLogic where
import           Test.QuickCheck

data Formula = Atom String
             | Not Formula
             | Formula `And` Formula
             | Formula `Or` Formula
             | Formula `Implies` Formula
             deriving (Show)

-- | pretty print a propositional logic formula
prettyPrint :: Formula -> String
prettyPrint = undefined

-- | parse a propositional logic formula
parse :: String -> Formula
parse = undefined

-- | generate a random formula
genRandom :: Int -> [String] -> Formula
genRandom numNodes atoms = undefined

-- | represent the formula in dimacs format
toDimacs :: Formula -> String
toDimacs = undefined

-- | check satisfiability of a formula using an external sat-solver
sat :: Formula -> IO Bool
sat = undefined

-- | check if a formula is a tautology (i.e. always true)
tautology :: Formula -> IO Bool
tautology = undefined

-- | check if two formulas are equivalent
equivalent :: Formula -> Formula -> IO Bool
equivalent = undefined


