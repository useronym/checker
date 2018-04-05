{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Config where

import           Data.Bitraversable    (bisequence)
import           Data.Function.Unicode
import           Options
import qualified Parse.Form            as F
import qualified Parse.Model           as M
import           Types


data MasterConfig = MasterConfig
  { model ∷ ValidatedModel
  , form  ∷ Form
  }

data SlaveConfig = SlaveConfig

data Config = Config
  { configPort ∷ String
  , configSpec ∷ Either MasterConfig SlaveConfig
  }


loadConfig ∷ Options → IO Config
loadConfig Options{..} =
  bisequence (either (Left ∘ loadConfigMaster) (Right ∘ loadConfigSlave) optionsSpec)
    >>= return ∘ (Config optionsPort)

loadConfigMaster ∷ MasterOptions → IO MasterConfig
loadConfigMaster MasterOptions{..} = do
  m ← M.parse modelPath
  return $ MasterConfig
    { model = fromRight m
    , form  = fromRight $ F.parse form
    }
  where fromRight (Left e)  = error (show e)
        fromRight (Right r) = r

loadConfigSlave ∷ SlaveOptions → IO SlaveConfig
loadConfigSlave SlaveOptions = return SlaveConfig
