- | parsing command line arguments
module ArgParser
  ( Args (..)
  , parseArgs
  ) where

import           Options.Applicative

data Args = Args
  { optimizedSearch :: Bool
  , lang            :: String
  , input           :: Input
  } deriving (Show)

parseArgs = execParser $
  info (args <**> helper)
    (fullDesc
    <> progDesc "Verifies refinement of probabilistic contracts"
    <> header "probabilistic-contracts")

args :: Parser Args
args = Args
  <$> strOption (long "lang"
            <> metavar "LANG"
            <> help "The specification language that the probabilistic contracts are written in. Can be either LTL or MLTL")
  <*> switch (long "compile"
            <> help "compile the program into an executable")
  <*> argument str (metavar "filename" <> help "the cigrid-program file to be compiled")

-- parse what specification language to use
data SpecLang = SpecLang String

parseLTLSpec :: Parser SpecLang
parseLTLSpec = str

-- parsing the formula, either from a file or from a string in the command call
-- or from stdin
data Input = FileInput String
           | ArgInput String
           | StdInput

parseFileInput :: Parser Input
parseFileInput = FileInput <$> strOption
  (long "file" <> metavar "FILENAME" <> help "use a refinement problem from a file")

parseArgInput :: Parser Input
parseArgInput = FileInput <$> strOption
  (long "formula" <> metavar "REFINEMENT PROBLEM" <> help "use a refinement problem written in this argument")



