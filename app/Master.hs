{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Control.Distributed.Process
import           Config
import           Control.Monad                                      (forM)
import           Data.Function.Unicode
import           Slave
import           Types
import           List


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster MasterConfig{model = m@PolyModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes: " ++ show slaves
  pids ← distribute (spawnSlave m ϕ)
  self ← getSelfPid
  mapM (`send` (self:pids)) pids
  await pids
  where distribute = let ss        = map polyId polyStates
                         workloads = partitionN (length slaves) ss
                     in forM (zip workloads slaves) ∘ uncurry

await ∷ [ProcessId] → Process ()
await pids = undefined
