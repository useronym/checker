{-# LANGUAGE RecordWildCards #-}
module Config where

import           Options
import           Parse
import           Types


data MasterConfig = MasterConfig
  { peers ∷ [String]
  , model ∷ Model
  , form  ∷ Form
  }

data SlaveConfig = SlaveConfig
  {
  }

type EitherConfig = Either MasterConfig SlaveConfig

data Config = Config
  { port       ∷ Int
  , modeConfig ∷ EitherConfig
  }

loadConfig ∷ Options → IO Config
loadConfig (Options shared options) = do
  s ← loadConfigShared shared
  o ← loadConfigEither options
  return $ s { modeConfig = o }

loadConfigShared ∷ SharedOptions → IO Config
loadConfigShared SharedOptions{..} = return $ Config
  { port = port
  , modeConfig = error "Uninitialized" }

loadConfigEither ∷ EitherOptions → IO EitherConfig
loadConfigEither (Master options) = loadConfigMaster options
loadConfigEither (Slave options)  = loadConfigSlave options

loadConfigMaster ∷ MasterOptions → IO EitherConfig
loadConfigMaster MasterOptions{..} = do
  m ← loadModel modelPath
  p ← loadPeers peersPath
  return $ Left $ MasterConfig
    { peers = p
    , model = fromRight m
    , form = fromRight $ parseForm form
    }
  where fromRight (Left e) = error (show e)
        fromRight (Right r) = r

loadConfigSlave ∷ SlaveOptions → IO EitherConfig
loadConfigSlave SlaveOptions = return $ Right $ SlaveConfig

loadPeers ∷ FilePath → IO [undefined]
loadPeers path = undefined
