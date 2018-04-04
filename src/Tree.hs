{-# LANGUAGE FlexibleInstances #-}

module Tree where

import           Control.Monad (liftM2)


-- Just a general tree structure, a.k.a. Multi-way or Rose tree.
data Tree a = Root [Tree a] | Node a [Tree a]
  deriving (Show)

instance Functor Tree where
  fmap f (Root ns)   = Root (map (fmap f) ns)
  fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

instance Foldable Tree where
  foldMap f (Root ns)   = mconcat $ map (foldMap f) ns
  foldMap f (Node x ns) = (f x) `mappend` (mconcat $ map (foldMap f) ns)

instance Traversable Tree where
  traverse f (Root ns)   = Root <$> (sequenceA $ map (traverse f) ns)
  traverse f (Node x ns) = Node <$> f x <*> (sequenceA $ map (traverse f) ns)


instance Monad m ⇒ Monoid (m Bool) where
  mempty  = return False
  mappend = liftM2 (||)
