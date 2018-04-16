{-# LANGUAGE FlexibleInstances #-}

module Tree where

import           Control.Monad         (liftM2)
import           Data.Function.Unicode


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


-- Tis is (𝔹, ∨, ⊥).
instance Monoid Bool where
  mempty = False
  True `mappend` _  = True
  False `mappend` x = x

data Three = Yes | No | Maybe
  deriving (Show)

instance Monoid Three where
  mempty = Maybe
  Yes `mappend` _   = Yes
  No `mappend` _    = No
  Maybe `mappend` x = x

instance (Monad m, Monoid a) ⇒ Monoid (m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

threeToBool ∷ Three → Bool
threeToBool Yes = True
threeToBool _   = False

caseM ∷ Monad m ⇒ [(m Bool, Three)] → m Three
caseM []                 = return No
caseM ((test, res):rest) = test >>= \t → if t then return res else caseM rest
