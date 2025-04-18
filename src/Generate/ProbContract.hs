module Generate.ProbContract where
import           Contracts.Probabilistic
import qualified Generate.LTL            as GenLTL
import qualified Specs.LTL               as LTL

-- | Generate a full refinement verification instance
genRefinementProblem :: Int -> Int -> Int -> String -> [ProbContracts]
genRefinementProblem numContracts contractSize numVars specLang = undefined

-- | generate a probabilistic contract with specifications written in LTL
genProbContractLTL :: [String] -> Int -> [String] -> [String] -> ProbContract LTL.Formula
genProbContractLTL cmps specSize invars outvars = undefined

-- | generate a probabilistic contract with specifications written in MTL
genProbContractLTL :: [String] -> Int -> [String] -> [String] -> ProbContract LTL.Formula
genProbContractLTL cmps specSize invars outvars = undefined

-- | generate a probabilistic contract with specifications written in Propositional logic
genProbContractLTL :: [String] -> Int -> [String] -> [String] -> ProbContract LTL.Formula
genProbContractLTL cmps specSize invars outvars = undefined



