{-# LANGUAGE FlexibleInstances #-}

module Contracts.Refinement.Reductions.Reduction where
import           Contracts.Refinement.Reductions.LinearEq
import qualified Solvers.LinearEq.Z3                      as Z3

class Reduction a where
  solve :: String -> a -> IO (Either String Bool)


instance Reduction [LinearEq] where
  solve solver eq = case solver of
    "z3" -> Z3.solve eq
    _    -> pure $ Left ("unknown solver: " <> solver)


