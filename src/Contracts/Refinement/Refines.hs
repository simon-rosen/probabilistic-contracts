module Contracts.Refinement.Refines where
import           Contracts.Probabilistic
import           Contracts.Refinement.Reductions.LinearEq  (LinearEq (..))
import qualified Contracts.Refinement.Reductions.Reduction as Reduction
import           Control.Exception                         (evaluate)
import           Control.Monad                             (filterM, mapM,
                                                            replicateM)
import           Data.Bits                                 ((.&.))
import           Data.List                                 (elemIndex)
import           Data.Maybe                                (fromJust)
import           Math
import qualified Specs.Solvable                            as Solvable
import           Specs.Solvable                            (Solvable)




-- | refinement verification - by a reduction to QF_LRA
-- check if a parallel composition of a set of probabilistic contracts
-- refines another probabilistic contract
-- If this function returns true then the refinement is correct,
-- if it returns false then it is unknown.
--
refines :: (Eq a, Solvable a) => String -> String ->
  ProbContract a -> [ProbContract a] -> IO (Either String Bool)
refines specSolver ineqSolver sysSpec componentSpecs = do
  -- step 1: calculate the nonempty variables
  eVars <- nonZeroVars specSolver (sysSpec : componentSpecs)
  case eVars of
    Left msg -> pure $ Left msg
    Right vars -> do
      _ <- evaluate (length vars) -- force evaluation before timing ends
      -- step 2: create system of linear inequalities
      let ineqs = createIneqs (sysSpec : componentSpecs) vars
      -- step 3: solve the equation system
      --let smt = toSMT vars ineqs
      --putStrLn smt
      --(Right solved) <- checkLineqs smt
      Right solved <- Reduction.solve ineqSolver ineqs
      pure $ Right (not solved)


createAndSolveIneqs :: (Eq a) => [ProbContract a] -> [Var] -> IO (Either String Bool)
createAndSolveIneqs pcs vars = do
  let ineqs = createIneqs pcs vars
  Reduction.solve "z3" ineqs

----------------------------------------------------------------------
-- These functions are used together later to simplify the step in the
-- algorithm where one writes the equations for each probabilistic
-- contract in the equation system. In this step one includes all variables
-- for the corresponding equation that include the full contract, and those
-- that include the assumption in the contract. By using the binary representation
-- of a variable as an encoding of which specs were negated (=0) and which were
-- not (=1) this step is simply bitwise and:ing.

-- Convert a variable (number) into its binary representation
-- (but reversed: 1010 = 1*2^0 + 0*2^1 + 1*2^2 + 0*2^3) with a certain length
var2bin :: Var -> Int -> [Int]
var2bin (NumberedVar _ v) len =
  let bin = dec2bin (fromIntegral v)
  in bin <> take (len - length bin) (repeat 0)
  where
    dec2bin :: Int -> [Int]
    dec2bin n
      | n == 0 = []
      | otherwise = let (q, r) = n `divMod` 2 in r : dec2bin q

-- A number that identifies a contract (Ai, Gi) in a list of contracts.
-- Ex: (A2, G2) in the list [A0, G0, A1, G1, A2, G2, A3, G3] will be
-- given the number binary number 00001100, which interpreted backwards
-- (this interpretation simplifies things) is
-- 0*2^0 + 0*2^1 + 0*2^2 + 0*2^3 + 1*2^4 + 1*2^5 + 0*2^6 + 0*2^7 = 48
agNum :: Int -> Integer
agNum i = 2^(2*i) + 2^(2*i + 1)

-- The same thing as above but only take into account the Assume contract.
-- Ex: A2 in the list [A0, G0, A1, G1, A2, G2, A3, G3] will be given the
-- binary number 00001000 which interpreted backwards is 2^4 = 16
aNum :: Int -> Integer
aNum i = 2^(2*i)

-- get the formula corresponding to a certain variable
varFormula :: (Complementable a, Intersectable a) => [ProbContract a] -> Var -> a
varFormula contracts var =
  -- convert the var into a list of booleans
  -- where True means that a spec should be itself
  -- and False means that it should be complemented
  let compbits = var2bin var (length contracts * 2)
  -- put the specifications from contracts [C0, C1, C2, C3]
  -- in a list [A0, G0, A1, G1, A2, G2, Al, G3]
      specs = concatMap (\(ProbContract p) -> let (a, g) = getProbEvent p in [a, g]) contracts
  -- complement all specs whose entry in bools is False
      specs' = zipWith
        (\spec compbit -> case compbit of; 1 -> spec; 0 -> complement spec) specs compbits
  -- return the conjunction of the specs
  in foldl intersect (head specs') (tail specs')

---------------------------------------------------------------------
-- | naive method to get all vars that are non-empty: check each var,
-- (there are 2^2n vars for n contracts)
nonZeroVars :: (Solvable a) => String -> [ProbContract a] -> IO (Either String [Var])
nonZeroVars solver pcs = do
  let vars = [NumberedVar "z" i | i <- [0..(2^(2 * length pcs) - 1)]]
  res <- mapM (\var -> Solvable.solve solver (varFormula pcs var)) vars
  case sequence res of
    Left err   -> pure $ Left err
    Right sats -> pure $ Right [var | (var, True) <- zip vars sats]

-- | maybe a better way of finding all non-zero vars. The zero vars can only happen
-- between dependencies?
{-linearZeroVars :: (Solvable a) => String -> [ProbContract a] -> IO (Either String [Var])
linearZeroVars solver pcs = do
  let
  where
    pairwise :: Int -> IO (Either String [Var])
    pairwise i = do
      let localcomps = [pcs !! i] <> [pcs !! (i+1)]
      let vars = [NumberedVar "z" i | i <- [0..(2^(2 * 2) - 1)]]
      res <- mapM (\var -> Solvable.solve solver (varFormula localcomps var)) vars
      case sequence res of
        Left err   -> pure $ Left err
        Right sats -> do
          let zeros = [var | (var, False) <- zip vars sats]
          -- now we pad the beginning and end
          let before = replicateM (i-1) [0,1]
          let after = replicateM (i-1) [0,1]
-}

-- | reduce refinement verification to solving a system of linear inequalities
createIneqs :: (Eq a) => [ProbContract a] -> [Var] -> [LinearEq]
createIneqs pcs vars =
  -- all vars sum to 1
  let sumto1 = [SumTo1 vars]
  -- all vars are non-negative
      nonneg = map (\var -> NonNeg var) vars
  -- the contract for the system shall be complemented
      pcs' = (complement $ head pcs) : tail pcs
  -- create the equations that relate to each prob-contract
      pcseqs = map (\pc -> createContractIneq vars (fromJust $ elemIndex pc pcs') pc) pcs'
  in sumto1 <> nonneg <> pcseqs

-- todo: check if this is correct
createContractIneq :: [Var] -> Int -> ProbContract a -> LinearEq
createContractIneq vars id (ProbContract (Probability _ c p)) =
  -- left side vars
  let lhs = filter (\(NumberedVar _ var) -> agNum (fromIntegral id) .&. var == agNum (fromIntegral id)) vars
  -- right side vars
      rhs = filter (\(NumberedVar _ var) -> aNum (fromIntegral id) .&. var == aNum (fromIntegral id)) vars
  in ContractEq lhs c p rhs
createContractIneq vars id (ProbContract (ProbabilityProd _ c ps)) =
  -- left side vars
  let lhs = filter (\(NumberedVar _ var) -> agNum (fromIntegral id) .&. var == agNum (fromIntegral id)) vars
  -- right side vars
      rhs = filter (\(NumberedVar _ var) -> aNum (fromIntegral id) .&. var == aNum (fromIntegral id)) vars
  in ContractEqProd lhs c ps rhs


