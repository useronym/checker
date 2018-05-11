{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Monad.ModelCheck (
    ModelCheck
  , ModelCheckState
  , Monad.ModelCheck.demote
  , put
  , putMany
  , processInput
  , processInputTimeout
  , withMap
  , lookupFailed
  , newModelCheckState
  , forever
  , Access(..)
  , evalIxStateT
  , evalIxStateT_
  , execIxStateT
  ) where

import           Control.Arrow         (first)
import           Control.Distributed.Process
import           Control.Monad.Indexed.State
import           Data.Function.Unicode
import qualified Data.HashMap                as H
import           Monad.Types
import           Prelude                     hiding (lookup, read)
import           Types


type Assignment = (SerRun,  Bool)

type Store α = IORefHashMap α SerRun Bool

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
put ∷ (Run, Bool) → ModelCheck a Bool
put (r, v) = broadcastMany [(toSer r, v)] >> return v

putMany ∷ [(Run, Bool)] → ModelCheck a ()
putMany rs = broadcastMany (first toSer <$> rs)

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

withMap ∷ (H.Map SerRun Bool → α) → ModelCheck AWrite α
withMap f = igets checkStore >>= liftIO ∘ (f <$>) ∘ getMap

-- Direct operations on the assignment store.
lookupFailed ∷ ModelCheck AWrite [SerRun]
lookupFailed = withMap $ H.keys ∘ (H.filter (==False))

insertMany ∷ [Assignment] → ModelCheck AWrite ()
insertMany as = withStore $ writeMany as

---- Convenience functions for beginning the computation.
newModelCheckState ∷ [ProcessId] → Process (ModelCheckState AWrite)
newModelCheckState peers = do
  store ← liftIO new
  return ModelCheckState {checkPeers = peers, checkStore = store}

-- Convenience functions for waiting for the computation to finish.
forever ∷ ModelCheck a () → ModelCheck a ()
forever pa = pa >> forever pa
