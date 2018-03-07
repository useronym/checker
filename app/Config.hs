{-# LANGUAGE RecordWildCards #-}
module Config where

import           Control.Monad         (sequence)
import           Data.Bitraversable    (bisequence)
import           Data.Function.Unicode
import           Options
import qualified Parse.Form            as F
import qualified Parse.Model           as M
import           Types


data MasterConfig = MasterConfig
  { masterPort ∷ String
  , model      ∷ ValidatedModel
  , form       ∷ Form
  }

data SlaveConfig = SlaveConfig
  { slavePort ∷ String
  }

type Config = Either MasterConfig SlaveConfig


loadConfig ∷ Options → IO Config
loadConfig = bisequence ∘ either (Left ∘ loadConfigMaster) (Right ∘ loadConfigSlave)

loadConfigMaster ∷ MasterOptions → IO MasterConfig
loadConfigMaster MasterOptions{..} = do
  m ← M.parse modelPath
  return $ MasterConfig
    { masterPort = masterPort
    , model = fromRight m
    , form = fromRight $ F.parse form
    }
  where fromRight (Left e)  = error (show e)
        fromRight (Right r) = r

loadConfigSlave ∷ SlaveOptions → IO SlaveConfig
loadConfigSlave SlaveOptions{..} = return $ SlaveConfig
  { slavePort = slavePort
  }
