module Monad.ModelCheck where

import           Control.Distributed.Process
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.State   hiding (State, get, put)
import           Data.Function.Unicode
import           Data.Maybe                  (fromJust)
import           Monad.Types
import           Prelude                     hiding (lookup, read)
import           Types


-- Assignment of a truth value to a formula at a state.
type Assignment = (State, Form, Bool)

-- The model checking monad allows access to & modification of truth values of formulae in the model.
-- Access is performed by a direct lookup in a shared state that resides on the current node.
-- Modification implies sending the new update to all nodes, including the current one.
type Store = IORefHashMap (State, Form) Bool

data ModelCheckState = ModelCheckState {
    checkPeers ∷ [ProcessId]
  , checkStore ∷ Store
  }

type ModelCheck = StateT ModelCheckState Process


-- Put a single assignment into the store. Returns the inserted value.
put ∷ Assignment → ModelCheck Bool
put a@(_, _, v) = putMany [a] >> return v

-- Or, put many.
putMany ∷ [Assignment] → ModelCheck ()
putMany as = do
  broadcastMany as
  insertMany as

-- Broadcast the new assgiments to all the peers.
broadcastMany ∷ [Assignment] → ModelCheck ()
broadcastMany a = withPeers $ mapM_ (`send` a)

-- Blockingly performs lookup in the assignment store. If the assignment we're looking for is not present,
-- we keep processing incoming assignments from other processes until it arrives.
get ∷ State → Form → ModelCheck Bool
get s ϕ = do
  x ← lookup s ϕ
  case x of
    Just a  → return a
    Nothing → do
      processInput
      get s ϕ

-- Processes all the incoming assignments and then attemps a lookup.
getMaybe ∷ State → Form → ModelCheck (Maybe Bool)
getMaybe s ϕ = do
  processRemainingInput
  lookup s ϕ

-- To process input, we first block until at least one input is available.
-- Then we process all the remaining input in the inbox.
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
lookup s ϕ = withStore $ read (s, ϕ)

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
newModelCheckState ps = do
  s ← liftIO new
  return ModelCheckState{checkPeers = ps, checkStore = s}

-- Convenience functions for waiting for the computation to finish.
untilM ∷ Monad m ⇒ m Bool → m () → m ()
untilM p a = p >>= \p' → if p'
                           then return ()
                           else a >> untilM p a

-- Repeatedly executes the action until there are `count` entries in the state.
untilFinished ∷ Int → ModelCheck () → ModelCheck ()
untilFinished count a = untilM (Monad.ModelCheck.size >>= \s → return $ s < count) a
