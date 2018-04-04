module Semantics where

import           Data.Function.Unicode
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Tree
import           Types


successors ∷ State → Tree State
successors = reachableWith stateNext

predecessors ∷ State → Tree State
predecessors = reachableWith statePrev

reachableWith ∷ (State → [State]) → State → Tree State
reachableWith f s = Root $ map (reachableWith' f [s]) (f s)

-- `col` keeps track of the states already collected on the path between
-- the current node and the root of the tree. In other words, states may
-- be duplicated in the tree, but on any given path from the root to a leaf,
-- every state is unique. This guarantees finitness.
reachableWith' ∷ (State → [State]) → [State] → State → Tree State
reachableWith' f col x = Node x $ map (reachableWith' f newcol) children
  where
    newcol   = x : col
    children = filter (not . (`elem` newcol)) (f x)


-- Unsafe.
getStateById ∷ Model → StateId → State
getStateById m s = fromJust $ lookupStateById m s

lookupStateById ∷ Model → StateId → Maybe State
lookupStateById (Model m) s = find ((== s) ∘ getStateId) m
