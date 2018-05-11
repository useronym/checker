{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
module Monad.Types where

import           Control.Monad.Indexed.State
import           Data.Function.Unicode
import           Data.Hashable
import           Data.HashMap
import           Data.IORef
import           Prelude                     hiding (lookup)


data Access = ARead | AWrite

-- An abstraction over a key-value store implementing a shared state
-- with concurrent support for one writer and many readers.
class SharedState (s ∷ Access → * → * → *) where
  new       ∷ IO (s AWrite k v)
  write     ∷ (Hashable k, Ord k) ⇒ k → v → s AWrite k v → IO ()
  read      ∷ (Hashable k, Ord k) ⇒ k → s a k v → IO (Maybe v)
  getMap    ∷ (Hashable k, Ord k) ⇒ s AWrite k v → IO (Map k v)
  writeMany ∷ (Hashable k, Ord k) ⇒ [(k, v)] → s AWrite k v → IO ()
  demote    ∷ s AWrite k v → s ARead k v
  writeMany kvs m = mapM_ (\(k, v) → write k v m) kvs


data IORefHashMap (a ∷ Access) k v = IORefHashMap (IORef (Map k v))

instance SharedState IORefHashMap where
  new                        = IORefHashMap <$> newIORef empty
  read k (IORefHashMap m)    = readIORef m >>= return ∘ lookup k
  write k v (IORefHashMap m) = modifyIORef' m (insert k v)
  getMap (IORefHashMap m)    = readIORef m >>= return
  demote (IORefHashMap m)    = IORefHashMap m


-- Things that should've been in indexed-extras.
lift ∷ Monad m ⇒ m a → IxStateT m i i a
lift ma = IxStateT $ (\i → ma >>= \a → return (a, i))

evalIxStateT ∷ Monad m ⇒ IxStateT m i j a → i → m a
evalIxStateT IxStateT{..} i = runIxStateT i >>= return ∘ fst

evalIxStateT_ ∷ Monad m ⇒ IxStateT m i j a → i → m ()
evalIxStateT_ m i = evalIxStateT m i >> return ()

execIxStateT ∷ Monad m ⇒ IxStateT m i j a → i → m j
execIxStateT IxStateT{..} i = runIxStateT i >>= return ∘ snd
