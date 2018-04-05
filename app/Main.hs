{-# LANGUAGE RecordWildCards #-}
module Main where

import           Config
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Master
import           Options                                            (parser)
import           Options.Applicative                                (execParser)
import           Slave                                              (__remoteTable)


main ∷ IO ()
main = execParser parser >>= loadConfig >>= run

run ∷ Config → IO ()
run Config{..} = (either runMaster runSlave configSpec) configPort

runMaster ∷ MasterConfig → String → IO ()
runMaster c@MasterConfig{..} port = do
  backend <- initializeBackend "127.0.0.1" port (__remoteTable initRemoteTable)
  startMaster backend (spawnMaster c)

runSlave ∷ SlaveConfig → String → IO ()
runSlave SlaveConfig port = do
  backend <- initializeBackend "127.0.0.1" port (__remoteTable initRemoteTable)
  putStrLn "Backend initialized, starting in slave mode."
  startSlave backend


