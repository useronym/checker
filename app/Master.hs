{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Data.Function.Unicode
import           List
import           Monad.ModelCheck
import           Parse.Model
import           Slave
import           Types


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster MasterConfig{model = m@PolyModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes (" ++ show (length slaves) ++ "): " ++ show slaves
  pids ← distribute (spawnSlave m ϕ)
  self ← getSelfPid
  mapM_ (`send` (self:pids)) pids
  res ← awaitResult (build m) ϕ
  liftIO $ putStrLn $ show res
  return ()
    where distribute = let ss        = map polyId polyStates
                           workloads = partitionN (length slaves) ss
                       in forM (zip workloads slaves) ∘ uncurry


awaitResult ∷ Model → Form → Process [(State, Bool)]
awaitResult m ϕ = newModelCheckState [] >>= stateServer ϕ >>= evalIxStateT (lookupAll m ϕ)
