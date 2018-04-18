{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Monad.ModelCheck (
    ModelCheck
  , ModelCheckState
  , Monad.ModelCheck.demote
  , put
  , processInput
  , processInputTimeout
  , lookup
  , lookupAll
  , Monad.ModelCheck.size
  , newModelCheckState
  , untilFinished
  , forever
  , Access(..)
  , evalIxStateT
  , evalIxStateT_
  , execIxStateT
  ) where

import           Control.Distributed.Process
import           Control.Monad.Extra         (mapMaybeM)
import           Control.Monad.Indexed.State
import           Data.Function.Unicode
import           Data.Maybe                  (fromJust)
import           Monad.Types
import           Prelude                     hiding (lookup, read)
import           Types


-- Assignment of a truth value to a formula at a state.
type Assignment = (StateId, Form, Bool)

type Store α = IORefHashMap α (StateId, Form) Bool

-- The model checking monad allows access to & modification of truth values of formulae in the model.
-- Access is performed by a direct lookup in a shared state that resides on the current node.
-- Modification implies sending the new update to all nodes, including the current one.
data ModelCheckState α = ModelCheckState {
    checkPeers ∷ [ProcessId]
  , checkStore ∷ Store α
  }

type ModelCheck α = IxStateT Process (ModelCheckState α) (ModelCheckState α)

demote ∷ ModelCheckState AWrite → ModelCheckState ARead
demote ModelCheckState{..} = ModelCheckState {
    checkPeers = checkPeers
  , checkStore = Monad.Types.demote checkStore
  }

-- Put a single assignment into the store. Returns the inserted value.
put ∷ (State, Form, Bool) → ModelCheck a Bool
put (State{..}, ϕ, b) = put' (stateId, ϕ, b)

put' ∷ Assignment → ModelCheck a Bool
put' a@(_, _, v) = broadcastMany [a] >> return v

-- Broadcast the new assgiments to all the peers.
broadcastMany ∷ [Assignment] → ModelCheck a ()
broadcastMany a = withPeers $ mapM_ (`send` a)

-- To process input, we first block until at least one input is available.
-- Then we process all the remaining input in the inbox.
processInput ∷ ModelCheck AWrite ()
processInput = processOneInput >> processInputTimeout 0

processOneInput ∷ ModelCheck AWrite ()
processOneInput = do
  a ← lift $ expect
  insertMany a

processInputTimeout ∷ Int → ModelCheck AWrite ()
processInputTimeout t = do
  a ← lift $ expectTimeout t
  case a of
    Just a' → do
      insertMany a'
      processInputTimeout 0
    Nothing → return ()

-- Convenience wrappers.
withPeers ∷ ([ProcessId] → Process ()) → ModelCheck a ()
withPeers f = igets checkPeers >>= lift ∘ f

withStore ∷ (Store a → IO α) → ModelCheck a α
withStore f = igets checkStore >>= liftIO ∘ f

-- Direct operations on the assignment store.
lookup ∷ State → Form → ModelCheck a (Maybe Bool)
lookup State{..} ϕ = withStore $ read (stateId, ϕ)

lookupAll ∷ Run → Form → ModelCheck a [(State, Bool)]
lookupAll (Run ss) ϕ = mapMaybeM (\s → lookup s ϕ >>= return ∘ fmap (s, )) ss

insert ∷ Assignment → ModelCheck AWrite ()
insert (s, ϕ, v) = withStore $ write (s, ϕ) v

insertMany ∷ [Assignment] → ModelCheck AWrite ()
insertMany as = withStore $ writeMany (map (\(a,b,c) → ((a,b),c)) as)

size ∷ ModelCheck a Int
size = withStore Monad.Types.size

---- Convenience functions for beginning the computation.
newModelCheckState ∷ [ProcessId] → Process (ModelCheckState AWrite)
newModelCheckState peers = do
  store ← liftIO new
  return ModelCheckState {checkPeers = peers, checkStore = store}

-- Convenience functions for waiting for the computation to finish.
untilM ∷ Monad m ⇒ m Bool → m () → m ()
untilM p a = p >>= \p' → if p'
                           then return ()
                           else a >> untilM p a

-- Repeatedly executes the action until there are `count` entries in the state.
untilFinished ∷ Int → ModelCheck a () → ModelCheck a ()
untilFinished count a = untilM (Monad.ModelCheck.size >>= \s → return $ s >= count) a

forever ∷ ModelCheck a () → ModelCheck a ()
forever pa = pa >> forever pa
