module Options where

import           Data.Semigroup      ((<>))
import           Options.Applicative


data MasterOptions = MasterOptions
  { peersPath ∷ FilePath
  , modelPath ∷ FilePath
  , form      ∷ String
  }

data SlaveOptions = SlaveOptions
  {
  }

data EitherOptions = Master MasterOptions | Slave SlaveOptions

data SharedOptions = SharedOptions
  { port ∷ Int
  }

data Options = Options SharedOptions EitherOptions


optionsParser ∷ Parser Options
optionsParser = Options <$> sharedOptionsParser <*> eitherOptionsParser

sharedOptionsParser ∷ Parser SharedOptions
sharedOptionsParser =
  SharedOptions
  <$> option auto
      ( long "port"
     <> short 'p'
     <> value 9000
     <> help "Port to use for communication between the nodes.")

eitherOptionsParser ∷ Parser EitherOptions
eitherOptionsParser = (Master <$> masterOptionsParser) <|> (Slave <$> slaveOptionsParser)

masterOptionsParser ∷ Parser MasterOptions
masterOptionsParser =
  MasterOptions
  <$> strOption
      ( long "peers"
     <> help "Path to a file with a list of IP addresses, separated by newline."
     <> value "peers")
  <*> strOption
      ( long "model"
     <> short 'm'
     <> help "Path to the model file.")
  <*> strArgument
      ( help "Formula to check.")

slaveOptionsParser ∷ Parser SlaveOptions
slaveOptionsParser = pure SlaveOptions

parser ∷ ParserInfo Options
parser = info (optionsParser <**> helper)
  ( fullDesc
 <> header "Unnamed Model Checker."
 <> progDesc "Starts the model checker in one of the two modes.")
