{-# LANGUAGE FlexibleInstances #-}

module Tree where

import           Control.Monad         (liftM2)
import           Data.Function.Unicode


-- Just a general tree structure, a.k.a. Multi-way or Rose tree.
data Tree a = Node a [Tree a]
  deriving (Show)

instance Functor Tree where
  fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

instance Foldable Tree where
  foldMap f (Node x ns) = (f x) `mappend` (mconcat $ map (foldMap f) ns)

instance Traversable Tree where
  traverse f (Node x ns) = Node <$> f x <*> (sequenceA $ map (traverse f) ns)


data Three = Yes | No | Maybe
  deriving (Show)

instance Monoid Three where
  mempty = No

  Yes `mappend` _   = Yes
  No `mappend` _    = No
  Maybe `mappend` x = x

instance (Monad m, Monoid a) ⇒ Monoid (m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

threeToBool ∷ Three → Bool
threeToBool Yes = True
threeToBool _   = False

caseM ∷ (Monad m, Monoid a) ⇒ [(m Bool, a)] → m a
caseM []                 = return mempty
caseM ((test, res):rest) = test >>= \t → if t then return res else caseM rest
