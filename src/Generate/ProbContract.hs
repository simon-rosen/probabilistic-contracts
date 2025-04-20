module Generate.ProbContract where
import           Contracts.Probabilistic
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import           Math
import qualified Specs.LTL               as LTL
import qualified Specs.MLTL              as MLTL
import           System.Random           (randomRIO)

-- | generate a random probabilistic contract P(A, G) <= 0.5 (just an example)
-- where A and G are written in LTL
randomWithLTL :: Bool -> Int -> Int -> IO (ProbContract LTL.Formula)
randomWithLTL strictIneq size numAtoms = do
  a <- GenLTL.random size numAtoms
  g <- GenLTL.random size numAtoms
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
