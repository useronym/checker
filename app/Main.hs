{-# LANGUAGE RecordWildCards #-}
module Main where

import           Config
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Master
import           Options                                            (parser)
import           Options.Applicative                                (execParser)


main ∷ IO ()
main = execParser parser >>= loadConfig >>= run

run ∷ Config → IO ()
run = either runMaster runSlave

runMaster ∷ MasterConfig → IO ()
runMaster c@MasterConfig{..} = do
  backend <- initializeBackend "127.0.0.1" masterPort initRemoteTable
  startMaster backend (spawnMaster c)

runSlave ∷ SlaveConfig → IO ()
runSlave SlaveConfig{..} = do
  backend <- initializeBackend "127.0.0.1" slavePort initRemoteTable
  startSlave backend


