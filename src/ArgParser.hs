-- | parsing command line arguments
--
-- inspired by https://learn-haskell.blog/05-glue/04-optparse.html
module ArgParser
  ( parseArgs
  , SubCommand (..)
  , Lang (..)
  , Input (..)
  , Timeout (..)
  , Generator (..)
  , Benchmarker (..)
  ) where

import           Options.Applicative
import           System.FilePath     (FilePath)
import           Text.Read           (readMaybe)

-- | I want to support two types of functionalities:
-- 1. Verifying refinement of problem instances
-- 2. Generating refinement problem instances
-- 3. Run a benchmark on a problem and store the result in a sqlite database
data SubCommand = Verify Lang Input Timeout Bool
                | Generate Generator
                | Benchmark Benchmarker
                deriving (Show)

-- | The tool supports contracts written in these languages
data Lang = LTL
          | MLTL
          deriving (Show)

-- | The input to the verification subcommand can be supplied from
-- 1. the command line as an argument
-- 2. the path to a file where the problem is written
data Input = ArgInput String
           | FileInput FilePath
           deriving (Show)

type Timeout = Maybe Integer

type RequiredTimeout = Integer

-- | a subcommand for the generate command, that holds information
-- on what to generate.
-- Generating LTL problem has the parameters
-- * number of components
-- * size of formulas
-- * number of atoms per I/O var set
-- Generating MLTL problems has the same parameters, but also
-- * the max timestamp for temporal operators
--
-- Both generators also support two types of generation:
-- 1. using common specification patterns (set the Bool to true)
-- 1. using fully random specifications (set the Bool to false)
data Generator = GenerateLTL Int Int Int Bool
               | GenerateMLTL Int Int Int Int Bool
               deriving (Show)

-- | a subcommand for the benchmark command. It has the same options as
-- Generator along with a timeout and a name for the sqlite database.
data Benchmarker = BenchmarkerLTL Int Int Int String RequiredTimeout Bool
                 | BenchmarkerMLTL Int Int Int Int String RequiredTimeout Bool
                 deriving (Show)

-- | run the argument parser
parseArgs :: IO SubCommand
parseArgs = execParser $
  info (helper <*> pSubCommand)
    ( fullDesc
      <> header "probabilistic-contracts"
      <> progDesc "verifies refinement of probabilistic contracts specified with LTL/MLTL"
    )

-- parsing subcommands
pSubCommand :: Parser SubCommand
pSubCommand = subparser
  (command "verify"
      (info
         (helper <*> pVerify)
         (progDesc "verify a refinement statement")
      )
    <> command "generate"
      (info
        (helper <*> pGenerate)
        (progDesc "generate a refinement problem")
      )
    <> command "benchmark"
      (info
        (helper <*> pBenchmark)
        (progDesc "benchmark on a refinement problem")
      )
  )

-- parsing verify subcommand
pVerify :: Parser SubCommand
pVerify = Verify <$> pLang <*> pInput <*> pTimeout <*> pOptFlag

pLang :: Parser Lang
pLang = option (eitherReader readLang)
        ( long "lang"
        <> metavar "LANGUAGE"
        <> help "language used in the contract (LTL/MLTL)"
        )
        where
          readLang s = case s of
            "LTL"  -> Right LTL
            "MLTL" -> Right MLTL
            _      -> Left $ "Invalid language: " <> s

pInput :: Parser Input
pInput = pArgInput <|> pFileInput

pTimeout :: Parser Timeout
pTimeout = optional $ option auto
    ( long "timeout"
    <> metavar "INT"
    <> help "sets a timeout for the verification algorithm"
    )

pRequiredTimeout :: Parser RequiredTimeout
pRequiredTimeout = option auto
    ( long "timeout"
    <> metavar "INT"
    <> help "sets a timeout for the verification algorithm"
    )

pArgInput :: Parser Input
pArgInput = ArgInput <$>
  strOption
    ( long "problem"
    <> metavar "REFINEMENT_PROBLEM"
    <> help "supply a refinement problem to be verified directly from the command line"
    )

pFileInput :: Parser Input
pFileInput = FileInput <$>
  strOption
    ( long "file"
    <> metavar "FILE"
    <> help "the file where the refinement problem is stored"
    )

pOptFlag :: Parser Bool
pOptFlag = switch
  ( long "opt"
  <> help "Use the optimization during refinement that only checks each connection"
  )

-- parsing generate subcommand
pGenerate :: Parser SubCommand
pGenerate = subparser
  ( command "LTL"
      (info (Generate <$> pGenerateLTL)
        (progDesc "Generate an LTL refinement problem"))
  <> command "MLTL"
      (info (Generate <$> pGenerateMLTL)
        (progDesc "Generate an MLTL refinement problem"))
  )

pGenerateLTL :: Parser Generator
pGenerateLTL = GenerateLTL
  <$> pNumComponents
  <*> pFormulaSize
  <*> pAtomsPerVar
  <*> pCommon

pGenerateMLTL :: Parser Generator
pGenerateMLTL = GenerateMLTL
  <$> pNumComponents
  <*> pFormulaSize
  <*> pAtomsPerVar
  <*> pMaxTime
  <*> pCommon

pCommon :: Parser Bool
pCommon = switch
  ( long "common"
  <> help "generate specifications based on common specification patterns"
  )

-- all options for generators
pNumComponents :: Parser Int
pNumComponents = option auto
  ( long "num-components"
  <> metavar "INT"
  <> help "the number of components in the generated problem"
  )

pFormulaSize :: Parser Int
pFormulaSize = option auto
  ( long "formula-size"
  <> metavar "INT"
  <> help "the size of the formulas of the contracts in the generated problem"
  )

pAtomsPerVar :: Parser Int
pAtomsPerVar = option auto
  ( long "atoms-per-var"
  <> metavar "INT"
  <> help "the number of atoms per I/O var set in the generated problem"
  )

pMaxTime :: Parser Int
pMaxTime = option auto
  ( long "max-time"
  <> metavar "INT"
  <> help "the maximum time for temporal operators in the generated problem"
  )

-- parsing benchmark subcommand
pBenchmark :: Parser SubCommand
pBenchmark = subparser
  ( command "LTL"
      (info (Benchmark <$> pBenchmarkerLTL)
        (progDesc "Run a benchmark on a random LTL refinement problem"))
  <> command "MLTL"
      (info (Benchmark <$> pBenchmarkerMLTL)
        (progDesc "Run a benchmark on a random MLTL refinement problem"))
  )

pBenchmarkerLTL :: Parser Benchmarker
pBenchmarkerLTL = BenchmarkerLTL
  <$> pNumComponents
  <*> pFormulaSize
  <*> pAtomsPerVar
  <*> pDatabaseName
  <*> pRequiredTimeout
  <*> pCommon

pBenchmarkerMLTL :: Parser Benchmarker
pBenchmarkerMLTL = BenchmarkerMLTL
  <$> pNumComponents
  <*> pFormulaSize
  <*> pAtomsPerVar
  <*> pMaxTime
  <*> pDatabaseName
  <*> pRequiredTimeout
  <*> pCommon

pDatabaseName :: Parser String
pDatabaseName = strOption
    ( long "database"
    <> metavar "FILE"
    <> help "Output benchmark results to the given SQLite database file"
    )


