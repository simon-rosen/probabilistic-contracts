module ProbContract where
import           Control.Monad     (filterM)
import           Data.Bits         ((.&.))
import           Data.List         (elemIndex)
import           Data.Maybe        (fromJust)
import           Logics.Operations

data ProbContract a = ProbContract (PS.ProbStmt (a, a))
  deriving (Show, Eq)

mkProbContract :: (a, a) -> String -> Double -> ProbContract a
mkProbContract (a, g) rel p = ProbContract (PS.ProbStmt (a, g) r p)
  where
  r = case rel of
    "<"  -> PS.Less
    "<=" -> PS.Leq
    "="  -> PS.Eq
    "!=" -> PS.Neq
    ">=" -> PS.Geq
    ">"  -> PS.Greater

-- | refinement verification - by a reduction to QF_LRA
-- check if a parallel composition of a set of probabilistic contracts
-- refines another probabilistic contract
-- If this function returns true then the refinement is correct,
-- if it returns false then it is unknown
refines :: (LogicOperations a, Eq a) => [ProbContract a] -> ProbContract a -> IO (Either String Bool)
refines componentSpecs sysSpec = do
  -- step 1: calculate the nonempty variables
  vars <- nonEmptyVars (sysSpec : componentSpecs)
  -- step 2: create system of linear inequalities
  let ineqs = createIneqs (sysSpec : componentSpecs) vars
  -- step 3: solve the equation system
  let smt = toSMT vars ineqs
  --putStrLn smt
  (Right solved) <- checkLineqs smt
  pure $ Right (not solved)

-- | convert a variable (number) into its binary representation
-- but using bools instead of 0 and 1. The binary representation
-- also have a certain length.
var2bin :: Var -> Int -> [Bool]
var2bin v len =
  let bin = map (==1) (reverse $ dec2bin v)
  in take (len - length bin) (repeat False) <> bin
  where dec2bin n
          | n == 0 = []
          | otherwise = let (q, r) = n `divMod` 2 in r : dec2bin q

-- the number corresponding to a probabilistic contract in a list of such contracts
-- ex: The number for (A2, G2) in the list [A3, G3, A2, G2, A1, G1, A0, G0]
-- is 00110000_2 = 12_10
contractNumber :: Int -> Int
contractNumber id =
  2^(2*id + 1) + 2^(2*id)

-- the number corresponding only to the assume spec in a list of specs
-- ex the number for A2 in the list [A3, G3, A2, G2, A1, G1, A0, G0]
-- is 00001000_2 = 8_10
contractAssumeNumber :: Int -> Int
contractAssumeNumber id = 2^(2*id + 1)

-- check if a certaing combination of specs (a var) is satisfiable
checkSat :: (LogicOperations a) => [ProbContract a] -> Var -> IO Bool
checkSat contracts var = do
  -- convert the var into a list of booleans
  -- where True means that a spec should be itself
  -- and False means that it should be complemented
  let bools = var2bin var (length contracts * 2)
  -- put the specifications from contracts [C3, C2, C1, C0]
  -- in a list [A3, G3, A2, G2, A1, G1, A0, G0]
      getSpecs (ProbContract (PS.ProbStmt (a, g) _ _)) = (a, g)
      specs = concatMap (\pc -> let (a, g) = getSpecs pc in [a, g]) contracts
  -- negate all specs whose entry in bools is False
      specs' = zipWith (\spec notNeg ->
        if notNeg then spec else lNeg spec) specs bools
  -- check satisfiability of the conjunction of the specs
  res <- lSat (foldl lAnd (head specs') (tail specs'))
  pure $ case res of
            Left err   -> error err
            Right bool -> bool

-- | naive method to get all vars that are non-empty: check each var
nonEmptyVars :: (LogicOperations a) => [ProbContract a] -> IO [Var]
nonEmptyVars pcs = filterM (checkSat pcs) [0..(2^(2 * length pcs) - 1)]


-- | reduce refinement verification to solving a system of linear inequalities
createIneqs :: (Eq a) => [ProbContract a] -> [Var] -> [LinearIneq]
createIneqs pcs vars =
  -- all vars sum to 1
  let sumto1 = [LeftSum vars PS.Eq 1.0]
  -- all vars are non-negative
      nonneg = map (\var -> Individual var PS.Geq 0.0) vars
  -- create the equations that relate to each prob-contract
      pcseqs = map (\pc -> createContractIneq vars (fromJust $ elemIndex pc pcs) pc) pcs
  in sumto1 <> nonneg <> pcseqs

createContractIneq :: [Var] -> Int -> ProbContract a -> LinearIneq
createContractIneq vars id (ProbContract (PS.ProbStmt _ r p)) =
  -- left side vars
  let lhs = filter (\var -> contractNumber id .&. var == contractNumber id) vars
  -- right side vars
      rhs = filter (\var -> contractAssumeNumber id .&. var == contractAssumeNumber id) vars
  in LeftAndScaledRightSum lhs r p rhs


