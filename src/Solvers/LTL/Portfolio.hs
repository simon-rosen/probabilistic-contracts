-- | this is the solver I intend to use: a portfolio solver
-- that runs all other solvers in parallell and once one solver
-- finisher it terminates all processess
module Solvers.LTL.Portfolio where

import qualified Solvers.LTL.Aalta        as Aalta
import qualified Solvers.LTL.Black        as Black
import qualified Solvers.LTL.Spot         as Spot
import           Specs.LTL


import           Control.Concurrent.Async
import           Data.Either              (isRight)
import           Data.List                (delete)


{-
type Solver = (String, Formula -> IO (Either String Bool))


portfolioSolver :: [Solver] -> Formula -> IO (Either String Bool)
portfolioSolver solvers formula = go [] solvers
  where
    go running [] = do
      -- No more solvers left and none succeeded
      return (Left "No solver returned Right Bool")
    go running (s:ss) = withAsync (s formula) $ \a -> do
      let running' = a : running
      -- Wait for any solver to finish
      (done, result) <- waitAny running'
      case result of
        Right _ -> return result  -- Got a good result
        _       -> go (delete done running') ss  -- Keep going with others


solve :: Solver
solve = portfolioSolver [ ("aalta", Aalta.solve)
                        , ("black", Black.solve)
                        , ("spot", Spot.solve)
                        ]

-}
