{-# LANGUAGE KindSignatures #-}
module Monad.Types where

import           Prelude                     hiding (lookup)
import           Data.HashMap
import           Data.Function.Unicode
import           Data.IORef
import           Data.Hashable


-- An abstraction over a key-value store implementing a shared state
-- with concurrent support for one writer and many readers.
class SharedState (s ∷ * → * → *) where
  new       ∷ IO (s k v)
  write     ∷ (Hashable k, Ord k) ⇒ k → v → s k v → IO ()
  read      ∷ (Hashable k, Ord k) ⇒ k → s k v → IO (Maybe v)
  writeMany ∷ (Hashable k, Ord k) ⇒ [(k, v)] → s k v → IO ()
  writeMany kvs m = mapM_ (\(k, v) → write k v m) kvs


data IORefHashMap k v = IORefHashMap (IORef (Map k v))

instance SharedState IORefHashMap where
  new                        = IORefHashMap <$> newIORef empty
  read k (IORefHashMap m)    = readIORef m >>= return ∘ lookup k
  write k v (IORefHashMap m) = modifyIORef' m (insert k v)
