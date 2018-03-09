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


stateServer ∷ Form → ModelCheckState → Process ModelCheckState
stateServer ϕ = let count = length (subformulae ϕ) in
  execModelCheck (untilFinished count processInput)

initSlave ∷ (ValidatedModel, Form, [StateId]) → Process ()
initSlave (model, ϕ, states) = do
  say "Connected."
  let m = build model
  let ss = map (getStateById m) states
  peers ← expect ∷ (Process [ProcessId])
  sharedState ← newModelCheckState peers
  say "Received peer information, starting work."
  forM_ ss
    (\s → spawnLocal (evalModelCheck_ (check m s ϕ) sharedState))
  stateServer ϕ sharedState
  return ()

remotable ['initSlave]

spawnSlave ∷ ValidatedModel → Form → [StateId] → NodeId → Process ProcessId
spawnSlave m ϕ ss node =
  spawn node ($(mkClosure 'initSlave) (m, ϕ, ss))
