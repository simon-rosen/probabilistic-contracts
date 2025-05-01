module Generate.ProbContract where
import           Contracts.Probabilistic
import           Control.Monad           (forM, replicateM)
import           Data.List.Split         (chunksOf)
import qualified Generate.LTL            as GenLTL
import qualified Generate.MLTL           as GenMLTL
import           Math
import qualified Specs.LTL               as LTL
import qualified Specs.MLTL              as MLTL
import           System.Random           (randomRIO)

-- | generate a random probabilistic contract P(A, G) <= 0.5 (just an example)
-- where A and G are written in LTL, and the inequalities are strict/non-strict
--
-- the assumption can use the atoms related to the input variables (aAtoms)
-- and the guarantee can use atoms related to both the input and the output
-- variables (aAtoms + gAtoms)
randomWithLTL :: Bool -> Bool -> Int -> ([LTL.Formula], [LTL.Formula]) -> IO (ProbContract LTL.Formula)
randomWithLTL isFirst strictIneq size (aAtoms, gAtoms) = do
  -- the first component should have Top as its input atoms
  a <- if isFirst
          then pure $ LTL.Top
          else GenLTL.random size aAtoms
  -- the output formula should have the input atoms + the output atoms
  -- (in the first component there is no input atoms)
  g <- if isFirst
          then GenLTL.random size gAtoms
          else GenLTL.random size (aAtoms <> gAtoms)
  c <- randomIneq strictIneq
  p <- randomRIO (0.0, 1.0) :: IO Double
  pure $ mkProbContract (a, g) c p

-- | generate a random probabilistic contract P(A, G) <= 0.5 (just an example)
-- where A and G are written in MLTL, and the inequalities are strict/non-strict
randomWithMLTL :: Bool -> Bool -> Int -> ([MLTL.Formula], [MLTL.Formula])-> Int -> IO (ProbContract MLTL.Formula)
randomWithMLTL isFirst strictIneq size (aAtoms, gAtoms) maxTime = do
  -- the first component should have Top as its input atoms
  a <- if isFirst
          then pure $ MLTL.Top
          else GenMLTL.random size aAtoms maxTime
  -- the output formula should have the input atoms + the output atoms
  -- (in the first component there is no input atoms)
  g <- if isFirst
          then GenMLTL.random size gAtoms maxTime
          else GenMLTL.random size (aAtoms <> gAtoms) maxTime
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

-- helper to generate the atoms for the I/O variables in each component
-- (there will be the same amount for each var and component)
--
-- each component must have the same input variables as the output variables
-- of the previous one, and the Input/Output variables for a component must be disjunct
-- (the assumption can only use atoms that talk about its input variables,
-- and the guarantee can use atoms that talk about both the input and output
-- variables).
-- ex: component vars = [(X1, X2), (X2, X3) (X3,X4)]
--     system vars = (X1, X4)
--     , where Xi is a set of variables
--     , I will generate some atoms for each set of variables
generateIOVarAtomsLTL :: Int -> Int -> [([LTL.Formula], [LTL.Formula])]
generateIOVarAtomsLTL numComponents numAtomsPerVar = let
  -- there are numComponents + 1 IO vars, and each have numAtomsPerVar atoms
  allAtoms = [ LTL.Atom ("p" <> show v)
             | v <- [1..numAtomsPerVar * (numComponents+1)]
             ]
  -- group all atoms belonging to the same I/O var
  atomsPerVar = chunksOf numAtomsPerVar allAtoms
  -- a function to put all the atoms for a components I/O vars
  -- in a tuple (i,o), such that each component gets the vars
  -- [(a1, a2), (a2, a3), (a3, a4), ...]
  -- where ai is a set of atoms
  createCompAtoms varsLeft = case varsLeft of
    (as1:as2:[])   -> [(as1, as2)]
    (as1:as2:left) -> (as1, as2) : createCompAtoms (as2:left)
  in createCompAtoms atomsPerVar

generateIOVarAtomsMLTL :: Int -> Int -> [([MLTL.Formula], [MLTL.Formula])]
generateIOVarAtomsMLTL numComponents numAtomsPerVar = let
  -- there are numComponents + 1 IO vars, and each have numAtomsPerVar atoms
  allAtoms = [ MLTL.Atom ("p" <> show v)
             | v <- [1..numAtomsPerVar * (numComponents+1)]
             ]
  -- group all atoms belonging to the same I/O var
  atomsPerVar = chunksOf numAtomsPerVar allAtoms
  -- a function to put all the atoms for a components I/O vars
  -- in a tuple (i,o), such that each component gets the vars
  -- [(a1, a2), (a2, a3), (a3, a4), ...]
  -- where ai is a set of atoms
  createCompAtoms varsLeft = case varsLeft of
    (as1:as2:[])   -> [(as1, as2)]
    (as1:as2:left) -> (as1, as2) : createCompAtoms (as2:left)
  in createCompAtoms atomsPerVar


-- | Generate a random refinement problem using contracts specified with LTL
-- args:
-- * numComponents - the number of components in the generated problem
-- * formulaSize - the size of each formula in the contracts
-- * numAtoms - the number of atoms for each I/O variable
randomRefinementProblemWithLTL :: Int -> Int -> Int -> IO (RefinementProblem LTL.Formula)
randomRefinementProblemWithLTL numComponents formulaSize atomsPerVar = do
  -- set up the atoms for the I/O variables on each component
  let componentVarsAtoms = generateIOVarAtomsLTL numComponents atomsPerVar
  -- a system contract, the inequality is strict
  -- and it uses the atoms from the input variables of the first component
  -- and the output of the putput variable of the last component
  sysContract <- randomWithLTL True True formulaSize
                  (fst $ head componentVarsAtoms, snd $ last componentVarsAtoms)
  -- component contracts, the inequalities are non-strict
  -- and they use their Input varisble for the assumption
  -- and their Input + Output variables for the guarantee
  firstCompContract <- randomWithLTL True False formulaSize
      (fst $ head componentVarsAtoms, snd $ head componentVarsAtoms)
  restCompContracts <- forM (tail componentVarsAtoms)
    (\varsAtoms -> randomWithLTL False False formulaSize varsAtoms)
  pure $ RefinementProblem
    { systemContract = sysContract
    , componentContracts = firstCompContract : restCompContracts
    }

-- | Generate a random refinement problem using contracts specified with MLTL
randomRefinementProblemWithMLTL :: Int -> Int -> Int -> Int -> IO (RefinementProblem MLTL.Formula)
randomRefinementProblemWithMLTL numComponents formulaSize atomsPerVar maxTime = do
  -- set up the atoms for the I/O variables on each component
  let componentVarsAtoms = generateIOVarAtomsMLTL numComponents atomsPerVar
  -- a system contract, the inequality is strict
  sysContract <- randomWithMLTL True True formulaSize
                  (fst $ head componentVarsAtoms, snd $ last componentVarsAtoms) maxTime
  -- componen contracts, the inequalities are non-strict
  -- and they use their Input variable for the assumption
  -- and their Input + Output variables for the guarantee
  firstCompContract <- randomWithMLTL True False formulaSize
      (fst $ head componentVarsAtoms, snd $ head componentVarsAtoms) maxTime
  restCompContracts <- forM (tail componentVarsAtoms)
    (\varsAtoms -> randomWithMLTL False False formulaSize varsAtoms maxTime)
  pure $ RefinementProblem
    { systemContract = sysContract
    , componentContracts = firstCompContract : restCompContracts
    }

