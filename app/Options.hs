module Options where

import           Data.Semigroup      ((<>))
import           Options.Applicative


data MasterOptions = MasterOptions
  { modelPath ∷ FilePath
  , output    ∷ Maybe FilePath
  , form      ∷ String
  }

data SlaveOptions = SlaveOptions {}

data Options = Options
  { optionsPort ∷ String
  , optionsSpec ∷ Either MasterOptions SlaveOptions
  }


optionsParser ∷ Parser Options
optionsParser = Options
  <$> strOption
      ( long "port"
     <> short 'p'
     <> value "9000"
     <> help "Port for this node to use.")
  <*> ((Left <$> masterOptionsParser) <|> (Right <$> slaveOptionsParser))

masterOptionsParser ∷ Parser MasterOptions
masterOptionsParser =
  MasterOptions
  <$> strOption
      ( long "model"
     <> short 'm'
     <> help "Path to the model file.")
  <*> optional (strOption
      ( long "out"
     <> short 'o'
     <> help "Path to the output file. If empty, stdout is used."))
  <*> strArgument
      ( help "Formula to check.")

slaveOptionsParser ∷ Parser SlaveOptions
slaveOptionsParser = pure SlaveOptions


parser ∷ ParserInfo Options
parser = info (optionsParser <**> helper)
  ( fullDesc
 <> header "Unnamed Model Checker."
 <> progDesc "Starts the model checker in one of the two modes.")
