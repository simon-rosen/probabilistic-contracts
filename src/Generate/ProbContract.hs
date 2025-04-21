module Generate.ProbContract where
import           Contracts.Probabilistic
import           Control.Monad           (replicateM)
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import           Math
import qualified Specs.LTL               as LTL
import qualified Specs.MLTL              as MLTL
import           System.Random           (randomRIO)

-- | generate a random probabilistic contract P(A, G) <= 0.5 (just an example)
-- where A and G are written in LTL, and the inequalities are strict/non-strict
randomWithLTL :: Bool -> Int -> Int -> IO (ProbContract LTL.Formula)
randomWithLTL strictIneq size numAtoms = do
  a <- GenLTL.random size numAtoms
  g <- GenLTL.random size numAtoms
  c <- randomIneq strictIneq
  p <- randomRIO (0.0, 1.0) :: IO Double
  pure $ mkProbContract (a, g) c p

-- | generate a random probabilistic contract P(A, G) <= 0.5 (just an example)
-- where A and G are written in MLTL, and the inequalities are strict/non-strict
randomWithMLTL :: Bool -> Int -> Int -> Int -> IO (ProbContract MLTL.Formula)
randomWithMLTL strictIneq size numAtoms maxTime = do
  a <- GenMLTL.random size numAtoms maxTime
  g <- GenMLTL.random size numAtoms maxTime
  c <- randomIneq strictIneq
  p <- randomRIO (0.0, 1.0) :: IO Double
  pure $ mkProbContract (a, g) c p


-- helper to generate a random comparasion operator - limited
-- to ["<", ">", "<=", ">="]
randomIneq :: Bool -> IO String
randomIneq strict = do
  op <- randomRIO (1, 2) :: IO Int
  pure $ case op of
    1 -> if strict then "<" else "<="
    2 -> if strict then ">" else ">="


-- | Generate a random refinement problem using contracts specified with LTL
randomRefinementProblemWithLTL :: Int -> Int -> Int -> IO (RefinementProblem LTL.Formula)
randomRefinementProblemWithLTL numComponents formulaSize numAtoms = do
  -- a system contract, the inequality is strict
  sysContract <- randomWithLTL True formulaSize numAtoms
  -- componen contracts, the inequalities are non-strict
  compContracts <- replicateM numComponents (randomWithLTL False formulaSize numAtoms)
  pure $ RefinementProblem
    { systemContract = sysContract
    , componentContracts = compContracts
    }

-- | Generate a random refinement problem using contracts specified with MLTL
randomRefinementProblemWithMLTL :: Int -> Int -> Int -> Int -> IO (RefinementProblem MLTL.Formula)
randomRefinementProblemWithMLTL numComponents formulaSize numAtoms maxTime = do
  -- a system contract, the inequality is strict
  sysContract <- randomWithMLTL True formulaSize numAtoms maxTime
  -- componen contracts, the inequalities are non-strict
  compContracts <- replicateM numComponents (randomWithMLTL False formulaSize numAtoms maxTime)
  pure $ RefinementProblem
    { systemContract = sysContract
    , componentContracts = compContracts
    }

