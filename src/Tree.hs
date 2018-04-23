module Tree where


-- Just a general tree structure, a.k.a. Multi-way or Rose tree.
data Tree a = Node a [Tree a]
  deriving (Show)

instance Functor Tree where
  fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

instance Foldable Tree where
  foldMap f (Node x ns) = (f x) `mappend` (mconcat $ map (foldMap f) ns)

instance Traversable Tree where
  traverse f (Node x ns) = Node <$> f x <*> (sequenceA $ map (traverse f) ns)
