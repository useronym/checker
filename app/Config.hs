{-# LANGUAGE RecordWildCards #-}
module Config where

import           Control.Monad         (sequence)
import           Data.Bitraversable    (bisequence)
import           Data.Function.Unicode
import           Options
import           Parse
import           Types


data MasterConfig = MasterConfig
  { peers      ∷ [String]
  , masterPort ∷ Int
  , model      ∷ Model
  , form       ∷ Form
  }

data SlaveConfig = SlaveConfig
  { slavePort ∷ Int
  }

type Config = Either MasterConfig SlaveConfig


loadConfig ∷ Options → IO Config
loadConfig = bisequence ∘ either (Left ∘ loadConfigMaster) (Right ∘ loadConfigSlave)

loadConfigMaster ∷ MasterOptions → IO MasterConfig
loadConfigMaster MasterOptions{..} = do
  m ← loadModel modelPath
  p ← loadPeers peersPath
  return $ MasterConfig
    { peers = p
    , masterPort = masterPort
    , model = fromRight m
    , form = fromRight $ parseForm form
    }
  where fromRight (Left e)  = error (show e)
        fromRight (Right r) = r

loadPeers ∷ FilePath → IO [undefined]
loadPeers path = undefined

loadConfigSlave ∷ SlaveOptions → IO SlaveConfig
loadConfigSlave SlaveOptions{..} = return $ SlaveConfig
  { slavePort = slavePort
  }
