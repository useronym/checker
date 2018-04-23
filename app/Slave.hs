{-# LANGUAGE TemplateHaskell #-}
module Slave (spawnSlave, __remoteTable, initSlave__tdict) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Monad                       (forM)
import           Data.List                           (delete)
import           List
import           ModelCheck.Simple
import           Monad.ModelCheck
import           Parse.Model
import           Semantics
import           SyncTypes
import           Types


worker ∷ ModelCheckState ARead → Model → [State] → Form → Process ()
worker state m ss ϕ = do
  evalIxStateT_ (mapM (\s → check m s ϕ) ss) state
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

initSlave ∷ (ProcessId, ValidatedModel, Form, [StateId]) → Process ()
initSlave (master, model, ϕ, states) = do
  let m  = build model
  let ss = map (getStateById m) states
  peers       ← expect ∷ Process [ProcessId]
  sharedState ← newModelCheckState peers
  let numWorkers = min (length ss) 32
  say $ "Received peer information, starting work on " ++ show numWorkers ++ " worker threads."
  refs ← forM (partitionN numWorkers ss) $ \ss' →
    spawnLocal (worker (demote sharedState) m ss' ϕ) >>= monitor
  monitorWorkers sharedState refs
  say "Workload finished."
  getSelfPid >>= \self → master `send` (SlaveFinished self)
  _ ← expect ∷ Process SlaveQuit
  return ()

remotable ['initSlave]

spawnSlave ∷ ProcessId → ValidatedModel → Form → [StateId] → NodeId → Process ProcessId
spawnSlave master m ϕ ss node =
  spawnLink node ($(mkClosure 'initSlave) (master, m, ϕ, ss))
