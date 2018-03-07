{-# LANGUAGE TemplateHaskell #-}
module Slave (spawnSlave) where

import           Control.Distributed.Process.Closure
import           Control.Distributed.Process
import           Types
import           Parse.Model


initSlave ∷ (ValidatedModel, Form, [StateId]) → Process ()
initSlave (model, ϕ, ss) = do
  let m = build model
  peers ← expect ∷ (Process [ProcessId])
  say "Received peer information, starting work."
  undefined

remotable ['initSlave]

spawnSlave ∷ ValidatedModel → Form → [StateId] → NodeId → Process ProcessId
spawnSlave m ϕ ss node =
  spawn node ($(mkClosure 'initSlave) (m, ϕ, ss))
