module Options where

import           Data.Semigroup      ((<>))
import           Options.Applicative


data MasterOptions = MasterOptions
  { peersPath  ∷ FilePath
  , masterPort ∷ String
  , modelPath  ∷ FilePath
  , form       ∷ String
  }

data SlaveOptions = SlaveOptions
  { slavePort ∷ String
  }

type Options = Either MasterOptions SlaveOptions


optionsParser ∷ Parser Options
optionsParser = (Left <$> masterOptionsParser) <|> (Right <$> slaveOptionsParser)

masterOptionsParser ∷ Parser MasterOptions
masterOptionsParser =
  MasterOptions
  <$> strOption
      ( long "peers"
     <> help "Path to a file with a list of IP addresses, separated by newline."
     <> value "peers")
  <*> strOption
      ( long "port"
     <> short 'p'
     <> value "9000"
     <> help "Port to use for communication between the nodes.")
  <*> strOption
      ( long "model"
     <> short 'm'
     <> help "Path to the model file.")
  <*> strArgument
      ( help "Formula to check.")

slaveOptionsParser ∷ Parser SlaveOptions
slaveOptionsParser =
  SlaveOptions
  <$> strOption
      ( long "port"
     <> short 'p'
     <> value "9000"
     <> help "Port to use for communication between the nodes.")


parser ∷ ParserInfo Options
parser = info (optionsParser <**> helper)
  ( fullDesc
 <> header "Unnamed Model Checker."
 <> progDesc "Starts the model checker in one of the two modes.")
