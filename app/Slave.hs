{-# LANGUAGE TemplateHaskell #-}
module Slave (spawnSlave, __remoteTable, initSlave__tdict) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Monad                       (forM)
import           Data.List                           (delete)
import           List
import           ModelCheck
import           Monad.ModelCheck
import           Parse.Model
import           SyncTypes
import           Types


worker ∷ ModelCheckState ARead → [Run] → Form → Process ()
worker state rs ϕ = do
  evalIxStateT_ (mapM (check ϕ) rs) state
  getSelfPid >>= flip exit "Workload finished."

monitorWorkers ∷ ModelCheckState AWrite → [MonitorRef] → Process ()
monitorWorkers state refs = do
  mNot   ← expectTimeout 0 ∷ Process (Maybe ProcessMonitorNotification)
  state' ← execIxStateT (processInputTimeout 1000) state
  let refs' = maybe refs handleNotif mNot
  if (null refs')
    then return ()
    else monitorWorkers state' refs'
  where
    handleNotif (ProcessMonitorNotification ref _ DiedNormal) = delete ref refs
    handleNotif (ProcessMonitorNotification _ _ reason)       = error $ "Worker died: " ++ show reason

initSlave ∷ (ProcessId, ValidatedModel, Form, Int, Int) → Process ()
initSlave (master, model, ϕ, from, count) = do
  let m  = build model
  let rs = makeAllRuns m
  let ss = slice rs from count
  peers       ← expect ∷ Process [ProcessId]
  sharedState ← newModelCheckState peers
  let numWorkers = min (length ss) 32
  say $ "Received peer information, starting work on " ++ show numWorkers ++ " worker threads."
  refs ← forM (partitionN numWorkers ss) $ \ss' →
    spawnLocal (worker (demote sharedState) ss' ϕ) >>= monitor
  monitorWorkers sharedState refs
  say "Workload finished."
  getSelfPid >>= \self → master `send` (SlaveFinished self)
  _ ← expect ∷ Process SlaveQuit
  return ()

remotable ['initSlave]

spawnSlave ∷ ProcessId → ValidatedModel → Form → (Int, Int) → NodeId → Process ProcessId
spawnSlave master m ϕ (i, j) node =
  spawnLink node ($(mkClosure 'initSlave) (master, m, ϕ, i, j))
