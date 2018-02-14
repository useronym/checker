{-# LANGUAGE UnicodeSyntax #-}
module Monad.ModelCheck where

import           Control.Distributed.Process
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.State   hiding (State, get, put)
import           Data.Function.Unicode
import qualified Data.HashTable.IO           as H
import           Data.Maybe                  (fromJust)
import           Prelude                     hiding (lookup)
import           Types


-- Assignment of a truth value to a formula at a state.
type Assignment = (State, Form, Bool)

-- The model checking monad allows access to & modification of truth values of formulae in the model.
-- Access is performed by a direct lookup in a hashtable assigned to this process.
-- Modification implies sending the new update to all other model checking processes.
type HashTable k v = H.BasicHashTable k v

data ModelCheckState = ModelCheckState {
    checkPeers ∷ [ProcessId]
  , checkState ∷ HashTable (State, Form) Bool
  }

type ModelCheck = StateT ModelCheckState Process

put ∷ Assignment → ModelCheck ()
put a = do
  broadcast a
  insert a

broadcast ∷ Assignment → ModelCheck ()
broadcast a = do
  ps ← gets checkPeers
  lift $ mapM_ (`send` a) ps

get ∷ State → Form → ModelCheck Bool
get s ϕ = do
  x ← lookup s ϕ
  case x of
    Just a → return a
    Nothing → do
      processInput
      get s ϕ

processInput = processOneInput >> processRemainingInput

processOneInput ∷ ModelCheck ()
processOneInput = do
  a ← lift $ expect
  insert a

processRemainingInput ∷ ModelCheck ()
processRemainingInput = do
  a ← lift $ expectTimeout 0
  case a of
    Just a' → do
      insert a'
      processRemainingInput
    Nothing → return ()

lookup ∷ State → Form → ModelCheck (Maybe Bool)
lookup s ϕ = do
  t ← gets checkState
  liftIO $ H.lookup t (s, ϕ)

insert ∷ Assignment → ModelCheck ()
insert (s, ϕ, v) = do
  t ← gets checkState
  liftIO $ H.insert t (s, ϕ) v
