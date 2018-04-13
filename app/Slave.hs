{-# LANGUAGE TemplateHaskell #-}
module Slave (spawnSlave, stateServer, __remoteTable) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Monad                       (forM_)
import           ModelCheck.Simple
import           Monad.ModelCheck
import           Parse.Model
import           Semantics
import           Syntax

import           Types


stateServer ∷ Int → Form → ModelCheckState AWrite → Process (ModelCheckState AWrite)
stateServer num ϕ = let count = num * length (subformulae ϕ) in
  execIxStateT (untilFinished count processInput)

initSlave ∷ (ValidatedModel, Form, [StateId]) → Process ()
initSlave (model, ϕ, states) = do
  say "Connected."
  let m = build model
  let ss = map (getStateById m) states
  peers ← expect ∷ (Process [ProcessId])
  sharedState ← newModelCheckState peers
  say "Received peer information, starting work."
  forM_ ss
    (\s → spawnLocal (evalIxStateT_ (check m s ϕ) (demote sharedState)))
  evalIxStateT_ (forever processInput) sharedState

remotable ['initSlave]

spawnSlave ∷ ValidatedModel → Form → [StateId] → NodeId → Process ProcessId
spawnSlave m ϕ ss node =
  spawn node ($(mkClosure 'initSlave) (m, ϕ, ss))
