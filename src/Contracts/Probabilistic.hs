module Contracts.Probabilistic where
import           Data.List (intersperse)
import           Math

-- | a probabilistic contract
data ProbContract a = ProbContract (Probability (a, a))
  deriving (Eq)

instance Complementable (ProbContract a) where
  complement (ProbContract p) = ProbContract (complement p)

instance Show a => Show (ProbContract a) where
  show (ProbContract (Probability (a, g) c p)) =
    "P(" <> show a <> ", " <> show g <> ") " <> show c <> " " <> show p

mkProbContract :: (a, a) -> String -> Double -> ProbContract a
mkProbContract (a, g) c p = ProbContract (Probability (a, g) (mkCompare c) p)


-- | a refinement problem for contracts written in specification language a:
-- Is a system level contract refined by the parallel composition of some
-- component contracts?
data RefinementProblem a =
  RefinementProblem { systemContract     :: ProbContract a
                    , componentContracts :: [ProbContract a]
                    }
                    deriving (Eq)

instance Show a => Show (RefinementProblem a) where
  show rp =
    show (systemContract rp) <> " ]= "
    <> (concat . intersperse " || " . map show) (componentContracts rp)
