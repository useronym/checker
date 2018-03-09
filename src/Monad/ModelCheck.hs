{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Monad.ModelCheck where

import           Control.Distributed.Process
import           Control.Monad.Extra         (mapMaybeM)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.State   hiding (State, get, put)
import           Data.Function.Unicode
import           Data.Maybe                  (fromJust)
import           Monad.Types
import           Prelude                     hiding (lookup, read)
import           Types


-- Assignment of a truth value to a formula at a state.
type Assignment = (StateId, Form, Bool)

-- The model checking monad allows access to & modification of truth values of formulae in the model.
-- Access is performed by a direct lookup in a shared state that resides on the current node.
-- Modification implies sending the new update to all nodes, including the current one.
type Store = IORefHashMap (StateId, Form) Bool

data ModelCheckState = ModelCheckState {
    checkPeers ∷ [ProcessId]
  , checkStore ∷ Store
  }

type ModelCheck = StateT ModelCheckState Process


-- Put a single assignment into the store. Returns the inserted value.
put ∷ (State, Form, Bool) → ModelCheck Bool
put (State{..}, ϕ, b) = put' (stateId, ϕ, b)

put' ∷ Assignment → ModelCheck Bool
put' a@(_, _, v) = broadcastMany [a] >> return v

-- Broadcast the new assgiments to all the peers.
broadcastMany ∷ [Assignment] → ModelCheck ()
broadcastMany a = do
  withPeers $ mapM_ (`send` a)

-- To process input, we first block until at least one input is available.
-- Then we process all the remaining input in the inbox.
processInput ∷ ModelCheck ()
processInput = processOneInput >> processRemainingInput

processOneInput ∷ ModelCheck ()
processOneInput = do
  a ← lift $ expect
  insertMany a

processRemainingInput ∷ ModelCheck ()
processRemainingInput = do
  a ← lift $ expectTimeout 0
  case a of
    Just a' → do
      insertMany a'
      processRemainingInput
    Nothing → return ()

-- Convenience wrappers.
withPeers ∷ ([ProcessId] → Process ()) → ModelCheck ()
withPeers f = gets checkPeers >>= lift ∘ f

withStore ∷ (Store → IO α) → ModelCheck α
withStore f = gets checkStore >>= liftIO ∘ f

-- Direct operations on the assignment store.
lookup ∷ State → Form → ModelCheck (Maybe Bool)
lookup State{..} ϕ = withStore $ read (stateId, ϕ)

lookupAll ∷ Model → Form → ModelCheck [(State, Bool)]
lookupAll (Model m) ϕ = mapMaybeM (\s → lookup s ϕ >>= return ∘ fmap (s, )) m

insert ∷ Assignment → ModelCheck ()
insert (s, ϕ, v) = withStore $ write (s, ϕ) v

insertMany ∷ [Assignment] → ModelCheck ()
insertMany as = withStore $ writeMany (map (\(a,b,c) → ((a,b),c)) as)

size ∷ ModelCheck Int
size = withStore Monad.Types.size

-- Convenience functions for beginning the computation.
evalModelCheck ∷ ModelCheck a → ModelCheckState → Process a
evalModelCheck = evalStateT

evalModelCheck_ ∷ ModelCheck a → ModelCheckState → Process ()
evalModelCheck_ a s = evalModelCheck a s >> return ()

execModelCheck ∷ ModelCheck a → ModelCheckState → Process ModelCheckState
execModelCheck = execStateT

newModelCheckState ∷ [ProcessId] → Process ModelCheckState
newModelCheckState peers = do
  store ← liftIO new
  return ModelCheckState {checkPeers = peers, checkStore = store}

-- Convenience functions for waiting for the computation to finish.
untilM ∷ Monad m ⇒ m Bool → m () → m ()
untilM p a = p >>= \p' → if p'
                           then return ()
                           else a >> untilM p a

-- Repeatedly executes the action until there are `count` entries in the state.
untilFinished ∷ Int → ModelCheck () → ModelCheck ()
untilFinished count a = untilM (Monad.ModelCheck.size >>= \s → return $ s >= count) a
