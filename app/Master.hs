{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Control.Monad.Extra         (ifM)
import           Control.Monad.Trans.Class   (lift)
import           Data.Function.Unicode
import           List
import           Monad.ModelCheck
import           Slave
import           Syntax
import           Types


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster MasterConfig{model = m@PolyModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes: " ++ show slaves
  pids ← distribute (spawnSlave m ϕ)
  self ← getSelfPid
  mapM (`send` (self:pids)) pids
  runAwait pids ϕ
    where distribute = let ss        = map polyId polyStates
                           workloads = partitionN (length slaves) ss
                       in forM (zip workloads slaves) ∘ uncurry


runAwait ∷ [ProcessId] → Form → Process ()
runAwait pids ϕ = newModelCheckState pids >>= evalModelCheck (await ϕ)

await ∷ Form → ModelCheck ()
await ϕ = do
  let count = length (subformulae ϕ)
  untilM (size >>= \s → do
             lift $ say $ "Progress: " ++ show s ++ "/" ++ show count
             return $ s < count)
    processInput
    where untilM p a = ifM p (return ()) (a >> untilM p a)
