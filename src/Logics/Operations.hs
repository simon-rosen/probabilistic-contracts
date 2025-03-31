module Logics.Operations where

class LogicOperations a where
  -- negate the formula of some logic
  lNeg :: a -> a
  -- and two formulas of some logic
  lAnd :: a -> a -> a
  -- determine satisfiability of a formula in some logic
  -- by calling an external solver for that logic
  lSat :: a -> IO (Either String Bool)


