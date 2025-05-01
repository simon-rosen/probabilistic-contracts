-- | parsing command line arguments
--
-- inspired by https://learn-haskell.blog/05-glue/04-optparse.html
module ArgParser
  ( parseArgs
  , SubCommand (..)
  , Lang (..)
  , Input (..)
  ) where

import           Options.Applicative
import           System.FilePath     (FilePath)


-- | I want to support two types of functionalities:
-- 1. Verifying refinement of problem instances
-- 2. Generating refinement problem instances
-- 3. Run benchmarking using a certein parameter space
data SubCommand = Verify Lang Input --Timeout
                | Generate Lang
                -- benchmark with parameters
                -- * specification languages to use (LTL,MLTL)
                -- * how many components
                -- * how many operators per formula
                -- * how many atoms per I/O var
                -- * max time (for MLTL only)
                -- | Benchmark [Lang] Int Int Int Int Timeout
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
  )

-- parsing verify subcommand
pVerify :: Parser SubCommand
pVerify = Verify <$> pLang <*> pInput

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

-- parsing generate subcommand
pGenerate :: Parser SubCommand
pGenerate = Generate <$> pLang







