{-# LANGUAGE TemplateHaskell #-}
module Slave (spawnSlave, __remoteTable) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Monad                       (forM)
import           Data.List                           (delete)
import           ModelCheck.Simple
import           Monad.ModelCheck
import           Parse.Model
import           Semantics
import           Syntax

import           Types


worker ∷ ModelCheckState ARead → Model → [State] → Form → Process ()
worker state m ss ϕ = do
  evalIxStateT_ (mapM (\s → check m s ϕ) ss) state
  getSelfPid >>= flip exit "Workload finished."

monitorWorkers ∷ ModelCheckState AWrite → [MonitorRef] → Process ()
monitorWorkers state refs = do
  mNot   ← expectTimeout 0 ∷ Process (Maybe ProcessMonitorNotification)
  state' ← execIxStateT (processInputTimeout 1000) state
  let refs' = maybe refs (\(ProcessMonitorNotification ref _ DiedNormal) → delete ref refs) mNot
  if (null refs')
    then return ()
    else monitorWorkers state' refs'

initSlave ∷ (ValidatedModel, Form, [StateId]) → Process ()
initSlave (model, ϕ, states) = do
  let m  = build model
  let ss = map (getStateById m) states
  peers       ← expect ∷ (Process [ProcessId])
  sharedState ← newModelCheckState peers
  say "Received peer information, starting work."
  refs ← forM ss $ \s →
    spawnLocal (worker (demote sharedState) m [s] ϕ) >>= monitor
  monitorWorkers sharedState refs
  say "Workload finished."

remotable ['initSlave]

spawnSlave ∷ ValidatedModel → Form → [StateId] → NodeId → Process ProcessId
spawnSlave m ϕ ss node =
  spawn node ($(mkClosure 'initSlave) (m, ϕ, ss))
